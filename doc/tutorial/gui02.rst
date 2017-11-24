GUI Basics
==========

We will now show how to do basic GUI programming.
The first thing we want to do is open a window!

.. code-block:: felix

    include "gui/__init__";
    open FlxGui;

    println$ "Basic Window Test";
    FlxGui::init();

    var w = create_resizable_window("Felix:gui_01_window_01",100,100,400,400);
    w.add$ mk_drawable FlxGuiSurface::clear lightgrey;
    w.update();
    w.show();

    sleep(15.0);

The Felix gui is not included by default, so we have to first
include the library with 

.. code-block:: felix

    include "gui/__init__";

Although this makes the library available, we would have to prefix
the names of all functions in the library with :code:`FlxGui::` to use them.
Since programmers hate typing stuff, we will open the library, so the
functions are all in the current scope and can be used without
the prefix.

.. code-block:: felix

    open FlxGui;

Next, we will print a diganostic to standard output so we know
which program is running, and then initialise library. 
Initialisation is a requirement imposed by SDL, which has a lot
of work to do on some platforms to connect to the GUI devices
such as the screen, touch (haptic) inputs, joysticks, mice,
keyboards, audio, and other multi-media hardware.

.. code-block:: felix

    println$ "Basic Window Test";
    FlxGui::init();

Now it it time for the fun! We will create a resizable window:

.. code-block:: felix

   var w = create_resizable_window("Felix:gui_01_window_01",100,100,400,400);

The first parameter is the title of the window, which should appear
on the titlebar (it doesn't on OSX! Because there is no pause to
accept input).

The next four parameters describe the window geometry.
The first two are the x and y coordinates. The SDL coordinate
system puts the origin in the top left corner, and x increases
down the screen. 

The unit of measurement is approximately the pixel.
I say approximately because on a Mac with a Retina display,
each pixel is often four displayt elements on the screen.
To confuse the issue, the Mac can do hardware scaling.
You'll just have to experiment!

The second two values are the width and height
of the window's client area, this does not include the title bar.
However the x and y coordinates are the top left corner of the
whole window including the title bar!

What we have created is a data structure representing the window.
The next thing we want to do is put some coloured pixels in it.

.. code-block:: felix

    w.add$ mk_drawable FlxGuiSurface::clear lightgrey;

This is an example of a fundamental operation, to add to a windows
display surface, the commands for drawing something.

The :code:`w.add` method adds a *drawable* to a list of drawables
kept for window :code:`w`.

The :code:`mk_drawable` method is a function which constructs a
drawable object. Its first argument is the actual drawing command,
:code:`FlxGuiSurface::clear` which clears a whole surface.
The second argument is an argument to that command, in this
case :code:`lightgrey`, which tells the clearing command what
colour to write on the surface it is clearing.

We have not actually drawn on the window at the point.
What we have done is packaged up the drawing instructions,
and *scheduled* them for drawing later.

To actually draw, we do this:

.. code-block:: felix

    w.update();

Now we have drawn the objects we scheduled to be drawn
on the systems internal representation of the window's
surface but still, nothing appears on the screen!

This is because the window has not be shown yet.
We've been drawing on it whilst it was invisible.
So we now make it visible:


.. code-block:: felix

    w.show();

FInally, we want the window to hang around for 15 seconds so
you can admire your fine art work. 

.. code-block:: felix

    sleep(15.0);

This causes the program to sleep for 15 seconds. The argument
is a double precision floating point number representing
a delay in seconds. The decimal point is mandatory and trailing
zero is mandatory!




 




