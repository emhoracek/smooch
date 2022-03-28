class Logger {
  constructor (logLevel) {
    if (['debug', 'warning', 'error'].includes(logLevel)) {
      this.logLevel = logLevel
    } else {
      this.logLevel = 'none'
    }
  }

  debug (msg) {
    if (this.logLevel !== 'none') {
      console.log(msg)
    }
  }

  warn (msg) {
    if (this.logLevel !== 'none' && this.logLevel !== 'debug') {
      console.log(msg)
    }
  }

  error (msg) {
    if (this.logLevel === 'error') {
      console.log(msg)
    }
  }
}

export { Logger }
