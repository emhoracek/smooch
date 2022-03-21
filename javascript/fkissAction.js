function makeAction (a, doll) {
  const actionMap = {
    altmap: mkAltmap,
    map: mkMap,
    sound: mkSound,
    timer: mkTimer,
    unmap: mkUnmap
    /*
    changecol
    changeset
    move
    nop
    shell
    sound
    transparent
    unmap
    viewport
    windowsize
    */
  }

  const mkAction = actionMap[a.action]

  if (mkAction) {
    return mkAction(a.args, doll)
  }
  doll.logger.warn(`Unknown action "${a.action}"`)
}

function mkTimer (args, doll) {
  const alarmId = args[0]
  const duration = args[1]
  return () => {
    doll.logger.debug(`Setting timer ${alarmId} for ${duration}ms`)
    const currentTimeout = doll.timers[alarmId].timeout
    if (currentTimeout) {
      clearTimeout(currentTimeout)
    }
    const timeout = setTimeout(() => {
      doll.timers[alarmId].timeout = false
      doll.timers[alarmId].callback()
    }, duration)

    doll.timers[alarmId].timeout = timeout
  }
}

function mkMap (args, doll) {
  const objOrCel = objOrCelArg(args[0], doll)
  if (objOrCel) {
    return objOrCel.map.bind(objOrCel)
  }
}

function mkUnmap (args, doll) {
  const objOrCel = objOrCelArg(args[0], doll)
  if (objOrCel) {
    return objOrCel.unmap.bind(objOrCel)
  }
}

function mkAltmap (args, doll) {
  const objOrCel = objOrCelArg(args[0], doll)
  if (objOrCel) {
    return objOrCel.altmap.bind(objOrCel)
  }
}

function objOrCelArg (arg, doll) {
  if (typeof arg === 'number') {
    const obj = doll.getObject(arg)
    if (obj) {
      return obj
    } else {
      doll.logger.warn(`Unable to find object with mark "#${arg}"`)
    }
  }
  if (arg.endsWith('.cel')) {
    const cel = doll.getCel(arg)
    if (cel) {
      return cel
    } else {
      doll.logger.warn(`Unable to find cel with filename "${arg}"`)
    }
  }
  doll.logger.error(`Expected a cel or object reference but got "${arg}"`)
}

export { makeAction, objOrCelArg }
