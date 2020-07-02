
# Description

A collection of recipes presented as an adventure game.

# Installation

You do not need to compile the program to use it, as it is already available [online](https://mbodin.github.io/tujkuko/).
Compiling the program yourself however provides an additional native version of the program, which is sensibly faster (but not as pretty as the online interface).

To compile the program, you will need to install `esy`.
One way to install `esy` is through npm:
```bash
npm install --global esy
```

Once installed, compile the project with `esy`:
```bash
esy
```

To locally run the web interface (available at [this address](https://mbodin.github.io/tujkuko/)), you need to set up a local server.
This README does not aim at explaining how to set it up; if you are using Github, you can push these changes online and access the corresponding [github.io](https://github.io) address.
If you have Python 2 installed, typing `esy server` will set up a local server: accessing `localhost:8000` should open the website.

One can generate documentation from the source as follows:
```bash
esy doc
```
This should create `_build/default/_doc/_html/index.html` containing the in-source documentation.
Alternatively, the generated documentation is available [here](https://mbodin.github.io/tujkuko/doc/).

# Licence

Copyright © 2020 Martin Constantino–Bodin

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

The program is under the GNU Affero General Public License version 3 (AGPLv3).
See the file [LICENSE](./LICENSE) for more information.

This program uses icons from [Pouf-Pouf Production’s SVG Inventory Icons](http://poufpoufproduction.fr/).
These icons are under GLPv3 and LGLPv3 licenses.

