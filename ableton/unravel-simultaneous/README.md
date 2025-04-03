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

