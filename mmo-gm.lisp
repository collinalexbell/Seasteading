(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(in-package #:cl-user)

(defpackage #:mmo-gm
  (:use #:cl)
  (:export #:start-all
           #:restart-all
           #:stop-all
           #:status
           #:start-repl
           #:start-runner
           #:stop-runner))

(in-package #:mmo-gm)

(defparameter *root* (truename #P"./"))
(defparameter *frontend-port* 8000)
(defparameter *tile-port* 8081)
(defparameter *http-pidfile* (merge-pathnames "server_http.pid" *root*))
(defparameter *tile-pidfile* (merge-pathnames "server.pid" *root*))
(defparameter *runner-pidfile* (merge-pathnames "runner.pid" *root*))
(defparameter *port-conflicts* (make-hash-table))
(defparameter *exe-signatures* (make-hash-table :test #'equal))

(defun read-pid (path)
  (when (probe-file path)
    (with-open-file (s path)
      (parse-integer (read-line s nil) :junk-allowed t))))

(defun port-pids (port)
  (let* ((cmd (format nil "lsof -t -i :~A" port))
         (out (uiop:run-program cmd :output :string :ignore-error-status t)))
    (when out
      (remove nil
              (mapcar (lambda (line)
                        (and (plusp (length line))
                             (parse-integer line :junk-allowed t)))
                      (uiop:split-string out :separator '(#\Newline #\Return)))))))

(defun record-conflict (port)
  (let ((pids (port-pids port)))
    (setf (gethash port *port-conflicts*) pids)))

(defun clear-conflict (port)
  (remhash port *port-conflicts*))

(defun exe-signature (path)
  (when (probe-file path)
    (let* ((tru (truename path))
           (size (with-open-file (s tru :direction :input :element-type '(unsigned-byte 8))
                   (file-length s)))
           (date (file-write-date tru)))
      (list tru size date))))

(defun sig-changed-p (key path)
  (let ((cur (exe-signature path))
        (prev (gethash key *exe-signatures*)))
    (if (equal cur prev)
        nil
        (progn (setf (gethash key *exe-signatures*) cur) (when prev t)))))

(defun clear-sigs ()
  (clrhash *exe-signatures*))

(defun write-pid (path pid)
  (with-open-file (s path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format s "~A~%" pid)))

(defun kill-pid (pid)
  (when pid
    (ignore-errors (uiop:run-program (list "kill" (princ-to-string pid))))))

(defun proc-alive-p (pid)
  (and pid (probe-file (format nil "/proc/~A" pid))))

(defun port-busy-p (port)
  (let* ((cmd (format nil "lsof -t -i :~A" port))
         (out (uiop:run-program cmd :output :string :ignore-error-status t)))
    (and out (plusp (length out)))))

(defun stop-all ()
  (let ((pids (remove nil (list (read-pid *http-pidfile*)
                                (read-pid *tile-pidfile*)
                                (read-pid *runner-pidfile*)))))
    (dolist (pid pids)
      (when (proc-alive-p pid)
        (kill-pid pid)
        (sleep 0.05))))
  ;; also kill anything listening on the known ports
  (dolist (port (list *frontend-port* *tile-port*))
    (let* ((cmd (format nil "lsof -t -i :~A" port))
           (out (uiop:run-program cmd :output :string :ignore-error-status t)))
      (when out
        (dolist (line (uiop:split-string out :separator '(#\Newline #\Return)))
          (let ((pid (and (plusp (length line))
                          (parse-integer line :junk-allowed t))))
            (when pid (kill-pid pid) (sleep 0.02)))))))
  ;; clear stale pidfiles
  (dolist (pf (list *http-pidfile* *tile-pidfile* *runner-pidfile*))
    (when (probe-file pf) (delete-file pf)))
  (clrhash *port-conflicts*)
  (clear-sigs)
  (values))

(defun status ()
  (let ((http (read-pid *http-pidfile*))
        (tile (read-pid *tile-pidfile*))
        (runner (read-pid *runner-pidfile*)))
    (format t "~%HTTP: ~A~%" (if (proc-alive-p http) http "stopped"))
    (format t "Tile: ~A~%" (if (proc-alive-p tile) tile "stopped"))
    (format t "Runner: ~A~%" (if (proc-alive-p runner) runner "stopped"))
    (maphash (lambda (port pids)
               (format t "Conflict on ~A -> ~A~%" port pids))
             *port-conflicts*)))

(defun start-http ()
  (uiop:with-current-directory (*root*)
    (let ((pid (read-pid *http-pidfile*)))
      (when (proc-alive-p pid) (return-from start-http pid)))
    (when (port-busy-p *frontend-port*)
      (record-conflict *frontend-port*)
      (return-from start-http nil))
    (clear-conflict *frontend-port*)
    ;; Delegate to existing script which writes server_http.pid
    (uiop:run-program (list "./start_server.sh")
                      :environment (list (format nil "PORT=~A" *frontend-port*))
                      :output :interactive :error-output :interactive)))

(defun ensure-server-binary ()
  (or (probe-file (merge-pathnames "server" *root*))
      (progn
        (uiop:run-program (list "make" "server") :output :interactive :error-output :interactive :directory *root*)
        (probe-file (merge-pathnames "server" *root*)))))

(defun ensure-runner-script ()
  (probe-file (merge-pathnames "ship-runner.js" *root*)))

(defun start-tile ()
  (uiop:with-current-directory (*root*)
    ;; if binary changed, restart tile server
    (when (sig-changed-p :server (merge-pathnames "server" *root*))
      (let ((pid (read-pid *tile-pidfile*)))
        (when (proc-alive-p pid) (kill-pid pid))))

    (let ((pid (read-pid *tile-pidfile*)))
      (when (proc-alive-p pid) (return-from start-tile pid)))
    (clear-conflict *tile-port*)
    (when (port-busy-p *tile-port*)
      (record-conflict *tile-port*)
      (return-from start-tile nil))
    (if (ensure-server-binary)
        (uiop:run-program (list "./start_tile_server.sh")
                          :environment (list (format nil "PORT=~A" *tile-port*))
                          :output :interactive :error-output :interactive)
        (error "Could not find or build server binary"))))

(defun start-runner ()
  (uiop:with-current-directory (*root*)
    (when (sig-changed-p :runner (merge-pathnames "ship-runner.js" *root*))
      (let ((pid (read-pid *runner-pidfile*)))
        (when (proc-alive-p pid) (kill-pid pid))
        (when (probe-file *runner-pidfile*) (delete-file *runner-pidfile*))))
    (let ((pid (read-pid *runner-pidfile*)))
      (when (proc-alive-p pid) (return-from start-runner pid)))
    (unless (ensure-runner-script)
      (error "ship-runner.js not found"))
    (uiop:run-program (list "./start_runner.sh")
                      :environment (list (format nil "SOCK=~A" "/tmp/ship_runner.sock"))
                      :output :interactive :error-output :interactive)))

(defun stop-runner ()
  (let ((pid (read-pid *runner-pidfile*)))
    (when (proc-alive-p pid) (kill-pid pid))
    (when (probe-file *runner-pidfile*) (delete-file *runner-pidfile*))))

(defun start-all (&key (frontend-port *frontend-port*) (tile-port *tile-port*))
  (setf *frontend-port* frontend-port
        *tile-port* tile-port)
  (start-http)
  (start-tile)
  (start-runner)
  (status))

(defun restart-all (&key (frontend-port *frontend-port*) (tile-port *tile-port*))
  (stop-all)
  (sleep 0.2)
  (start-all :frontend-port frontend-port :tile-port tile-port))

(defun ensure-quicklisp ()
  (unless (find-package :ql)
    (let ((setup (merge-pathnames ".quicklisp/setup.lisp" (user-homedir-pathname))))
      (when (probe-file setup)
        (load setup)))))

(defun start-repl (&key (port 4005))
  (ensure-quicklisp)
  (ql:quickload :swank)
  (funcall (read-from-string "swank:create-server") :port port :dont-close t))
