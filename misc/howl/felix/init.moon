howl.mode.register
  name: 'felix'
  aliases: {'flx'}
  extensions: {'flx'}
  create: -> bundle_load 'flx_mode'
  parent: 'curly_mode'

howl.mode.register
  name: 'fdoc'
  aliases: {'interscript'}
  extensions: {'fdoc'}
  create: -> bundle_load 'fdoc_mode'

unload = ->
  howl.mode.unregister mode for mode in *{'felix', 'fdoc'}

{
  info:
    author: 'Ryan Gonzalez'
    description: 'A Felix bundle'
    license: 'MIT'
  :unload
}
