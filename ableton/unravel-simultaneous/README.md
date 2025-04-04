# `UnravelSimultaneous`

This is a Max for Live device for use in Ableton Live, which unravels the
signals from the [FCB1010](https://www.behringer.com/product.html?modelCode=0715-AAA#),
using the [EurekaPROM](https://www.eurekasound.com/eurekaprom) chip in
[IO mode](https://www.eurekasound.com/eurekaprom/io), as much as possible.

Example:

Action          | Device sends     | Corrected signal
--------------- | ---------------- | ----------------
Press pedal 1   | Press 1 [104, 1] | Press 1 [104, 1]
Press pedal 5   | Press 5 [104, 5] | Press 5 [104, 5]
Release pedal 1 | _nothing_        | _nothing_
Press pedal 3   | _nothing_        | _nothing_
Release pedal 5 | _nothing_        | Release 1 [105, 1], Press 3 [104, 3], Release 5 [105, 5]

## Usage

Place the `UnravelSimultaneous` device on the track on which you want to use
it (for example, a piano track). Then place the `UnravelSimultaneousResetCC`
device on an unused other MIDI track, which contains only this device. Set the
output of this second track to be the first track.

## Background on `UnravelSimultaneousResetCC`

Ableton Live annoyingly filters out repeated CC messages when recording; this
means that without some kind of work-around, the first time pedal 1 is pressed
the corresponding `[104, 1]` message is processed just fine, but the second time
it is ignored, and the pedal effectively stops working (unless another pedal is
pressed first). Other people have complained about this also
(e.g. [1](https://forum.ableton.com/viewtopic.php?t=205747),
[2](https://forum.ableton.com/viewtopic.php?t=182949),
[3](https://forum.ableton.com/viewtopic.php?t=90046),
[4](https://cycling74.com/forums/no-continuous-cc-messages-from-midiin-and-ctlin)
),
but it seems there is
no way to stop Ableton from doing this.

To work around this problem, the `UnravelSimultaneous` device sends a message
to the `UnravelSimultaneousResetCC` device every time a pedal is pressed or
released. The `UnravelSimultaneousResetCC` device then emits a `[104, 127]` or
`[105, 127]` MIDI message; since this is routed to the main track that is
processing the signals from the FCB1010, it looks to Ableton as if the "value"
of CC 104/105 is changed to 127, so that a subsequent pedal press or release
looks like a change of value again and the signal is not ignored.