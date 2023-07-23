# sudoku-solver

First time making a webapp in Clojure. 

The source is a huge mess, but everytime I think about cleaning it up I realize I made so many wrong choices it'd be easier to just rewrite. And now I did, but not in way that interfaces with the rest of this app. If you just want to run a server to solve your sudoku problems, this app still works. If you want to look at some pretty source code to accomplish that, look [here.](https://gist.github.com/kawpuh/b7a842af2ca24e8a0c382b88ed1bbb62)
## Usage

```
lein run [port-number]
```
You will see the input page:  
![Input page](./img/input_demo.jpg)  
Clicking solve will bring you to:  
![Output page](./img/output_demo.jpg)  

The solver uses an adaptation of Peter Norvig's solver described [here.](https://norvig.com/sudoku.html)
## License

Copyright © 2019 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
