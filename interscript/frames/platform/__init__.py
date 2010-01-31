#line 14 "interscript/src/platform_frame.ipk"
import os
os_name = os.name
exec('from interscript.frames.platform.'+os_name+' import platform_frame')
platform = platform_frame()

