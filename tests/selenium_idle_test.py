"""
Headless Selenium sanity check: player ship should NOT move without input.

Prereqs:
  - Servers running on localhost:8000 (http) and :8081 (tile/runner).
  - Chromedriver/Chrome available on PATH.
  - venv with selenium installed (.venv).
"""

import time
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC


def get_state(driver):
    return driver.execute_script("return window.__lastState || {}")


def main():
    opts = Options()
    opts.add_argument("--headless=new")
    opts.add_argument("--disable-gpu")
    opts.add_argument("--no-sandbox")
    driver = webdriver.Chrome(options=opts)
    try:
        driver.set_page_load_timeout(15)
        driver.get("http://localhost:8000/")
        WebDriverWait(driver, 10).until(
            EC.presence_of_element_located((By.CSS_SELECTOR, "canvas"))
        )
        time.sleep(2)  # allow initial WS state
        state0 = get_state(driver)
        player0 = state0.get("player", {}) if state0 else {}
        x0, y0 = float(player0.get("x", 0)), float(player0.get("y", 0))

        time.sleep(3)  # no input; allow physics loop to run

        state1 = get_state(driver)
        player1 = state1.get("player", {}) if state1 else {}
        x1, y1 = float(player1.get("x", 0)), float(player1.get("y", 0))

        dx, dy = abs(x1 - x0), abs(y1 - y0)
        threshold = 5.0  # allow minor jitter
        idle_ok = dx < threshold and dy < threshold
        print(f"Idle movement acceptable? {idle_ok} (dx={dx:.2f}, dy={dy:.2f})")
    finally:
        driver.quit()


if __name__ == "__main__":
    main()
