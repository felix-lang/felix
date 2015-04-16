howl.mode.register
  name: 'felix'
  aliases: {'flx'}
  extensions: {'flx'}
  create: -> bundle_load 'mode'
  parent: 'curly_mode'

unload = -> howl.mode.unregister 'felix'

{
  info:
    author: 'Ryan Gonzalez'
    description: 'A Felix bundle'
    license: 'MIT'
  :unload
}
