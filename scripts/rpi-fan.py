#!/usr/bin/python
import sys
import time
import RPi.GPIO as GPIO

# Source: https://fizzy.cc/raspberry-pi-fan/
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

FAN_PIN = 18

def cpu_temp():
    with open("/sys/class/thermal/thermal_zone0/temp", 'r') as f:
        return float(f.read())/1000


def main():
    GPIO.setmode(GPIO.BCM)
    GPIO.setwarnings(False)
    GPIO.setup(FAN_PIN,GPIO.OUT)

    is_close = True
    GPIO.output(FAN_PIN,GPIO.LOW)

    print("Starting temprature monitring and fan control.")

    while True:
        temp = cpu_temp()
        if is_close:
            if temp > 55.0:
                print(time.ctime(), temp, 'Fan ON', file=sys.stderr)
                GPIO.output(FAN_PIN,GPIO.HIGH)
                is_close = False
        else:
            if temp < 48.0:
                print(time.ctime(), temp, 'Fan OFF', file=sys.stderr)
                GPIO.output(FAN_PIN,GPIO.LOW)
                is_close = True

        time.sleep(5.0)

        print(time.ctime(), temp, file=sys.stderr)


if __name__ == '__main__':
    main()
