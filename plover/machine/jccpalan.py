# Copyright (c) 2010-2011 Joshua Harlan Lifton.
# See LICENSE.txt for details.

"""Thread-based monitoring for Jack Chidley's Palan keyboard"""

import binascii

from plover import log
from plover.machine.base import SerialStenotypeBase


# In the Gemini PR protocol, each packet consists of exactly six bytes
# and the most significant bit (MSB) of every byte is used exclusively
# to indicate whether that byte is the first byte of the packet
# (MSB=1) or one of the remaining five bytes of the packet (MSB=0). As
# such, there are really only seven bits of steno data in each packet
# byte. This is why the STENO_KEY_CHART below is visually presented as
# six rows of seven elements instead of six rows of eight elements.
STENO_KEY_CHART = (
                    "P-", "M-", "N-", "-N", "-M", "-P",
                    "C-", "T-", "F-", "L-", "-L", "-F", "-T", "-H",
                    "S-", "H-", "R-", "Y-", "O-", "I", "-A", "-C", "-R", "-+", "-S",
                    "+1-", "+2-", "E-", "I", "-U", "-^1", "-^2"
                   )

BYTES_PER_STROKE = 6


class JCCPalan(SerialStenotypeBase):
    """Palantype interface for Jack Chidley's Palan keyboard
    """

    KEYS_LAYOUT = '''
           P- M- N-         -N -M -P
        C- T- F- L-         -L -F -T -H
        S- H- R- Y- O- I -A -C -R -+ -S
          +1-  +2-  E- I -U  -^1  -^2
    '''

    def run(self):
        """Overrides base class run method. Do not call directly."""
        self._ready()
        for packet in self._iter_packets(BYTES_PER_STROKE):
            if not (packet[0] & 0x80) or sum(b & 0x80 for b in packet[1:]):
                log.error('discarding invalid packet: %s',
                          binascii.hexlify(packet))
                continue
            steno_keys = []
            for i, b in enumerate(packet):
                for j in range(1, 8):
                    if (b & (0x80 >> j)):
                        steno_keys.append(STENO_KEY_CHART[i * 7 + j - 1])
            steno_keys = self.keymap.keys_to_actions(steno_keys)
            if steno_keys:
                self._notify(steno_keys)
