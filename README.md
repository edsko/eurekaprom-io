# Interface to the EurekaPROM in IO mode

This provides a library for easy communication with the
[EurekaPROM](https://www.eurekasound.com/eurekaprom) in IO mode, as well as two
command line applications: one to generate specific MIDI sequences to send to
the device to control the LEDs, and one turn inputs from the device into
programmable keyboard and/or mouse sequences, so that it can be used to control
general applications.

Requires [`alsa-seq`][https://hackage.haskell.org/package/alsa-seq], which in
turn requires the ALSA dev system libraries; see https://wiki.haskell.org/ALSA.
