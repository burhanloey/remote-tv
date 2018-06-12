# Remote TV

A website to upload video to Raspberry Pi and then play it. Apparently on my 512mb ram Pi, the upload took ages.
Probably the wireless dongle.

## Requirements

* Raspbian
* SBCL ARMhf
* quicklisp

## Setup

git clone this repository to home directory (`~/`).

cd to the repository, and run `./start.sh`.

The latest Raspberry Pi by default should have avahi-daemon already installed, so you can access the website at
`<hostname>.local:port`, for e.g `raspberrypi.local:3000`, provided that you are connected to the same router as
the Raspberry Pi.
