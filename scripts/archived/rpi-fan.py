#!/usr/bin/python
import RPi.GPIO as GPIO
import signal
import sys
import time

from datetime import datetime

# Recommend setting up a systemd service like this:
# /etc/systemd/system/fan-control.service
#
# [Unit]
# Description=Control the RPi Fan
#
# [Service]
# User=root
# WorkingDirectory=/tmp
# ExecStart=/home/rohan/configs/scripts/rpi-fan.py
# Restart=always
# Environment=PYTHONUNBUFFERED=1
# 
# [Install]
# WantedBy=multi-user.target


MIN_LOW = 40
MAX_HIGH = 65
DEFAULT_HIGH = 60

class FanControl:

    monitoring = True
    last_event = None
    fan_pin = None
    low = None
    high = DEFAULT_HIGH

    def __init__(self, fan_pin, high=None, low=None):
        self.fan_pin = fan_pin

        GPIO.setmode(GPIO.BCM)
        GPIO.setwarnings(False)
        GPIO.setup(self.fan_pin, GPIO.OUT)

        if high:
            self.high = min(high, MAX_HIGH)
        self.low = self.high - 10

        if low:
            self.low = max(min(self.low, low), MIN_LOW)

        signal.signal(signal.SIGINT, self.exit)
        signal.signal(signal.SIGTERM, self.exit)
        self.last_event = datetime.now()
        self.on()

    @property
    def is_on(self):
        return GPIO.input(self.fan_pin) == 1

    @property
    def temp(self):
        with open("/sys/class/thermal/thermal_zone0/temp", 'r') as f:
            return float(f.read())/1000

    @property
    def state(self):
        return 'On' if self.is_on else 'Off'

    def pretty_state(self):
        return "{} @ {:0.2f}'C".format(self.state, self.temp)

    def watch(self):

        while self.monitoring:
            temp = self.temp

            if temp >= self.high:
                self.on()
            elif temp < self.low:
                self.off()
            time.sleep(2.0)

        print('Done!', file=sys.stderr)

    def exit(self, _signum, _frame):
        self.off()
        GPIO.cleanup()
        self.monitoring = False
        print('Exiting...', file=sys.stderr)

    def duration(self, prev_state):
        if not self.last_event:
            return 'Duration N/A'

        last_event = self.last_event
        now = datetime.now()
        self.last_event = now

        return 'Was {} for: {}'.format(prev_state, (now - last_event))

    def off(self):
        if not self.is_on:
            return

        prev_state = self.state
        GPIO.output(self.fan_pin, GPIO.LOW)

        print("{} ({})".format(
            self.pretty_state(),
            self.duration(prev_state)
        ), file=sys.stderr)

    def on(self):
        if self.is_on:
            return

        prev_state = self.state
        GPIO.output(self.fan_pin, GPIO.HIGH)

        print("{} ({})".format(
            self.pretty_state(),
            self.duration(prev_state)
        ), file=sys.stderr)

if __name__ == '__main__':
    print("Starting temprature monitring and fan control...")
    controller = FanControl(12, 60)
    controller.watch()
