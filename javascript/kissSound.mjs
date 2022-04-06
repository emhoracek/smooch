class KiSSSound {
  constructor (staticDirectory, filename) {
    // some early viewers supported au and others supported wav
    // so early sets would ship with both
    // we can only play wav, so if we get an au file, we try
    // to find a wav version.
    // This is just a hack until Smooch supports multiple CNFs
    const wavFile = filename.replace('.au', '.wav')
    const url = `${staticDirectory}/${wavFile}`
    this.audio = new Audio(url)

    return this
  }

  play () {
    if (this.audio.HAVE_ENOUGH_DATA) {
      return this.audio.play()
    } else {
      this.audio.addEventListener('canplaythrough', event => {
        return this.audio.play()
      })
    }
  }
}

export { KiSSSound }
