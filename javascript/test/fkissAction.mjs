/* eslint-disable no-unused-expressions */
/* globals describe, it */
import { expect } from 'chai'
import { makeAction, objOrCelArg } from '../fkissAction.mjs'
import { Logger } from '../logger.mjs'

const testObj = {
  altmap () { return 'altmapped' },
  map () { return 'mapped' },
  unmap () { return 'unmapped' }
}
const testCel = {
  altmap () { return 'altmapped' },
  map () { return 'mapped' },
  unmap () { return 'unmapped' }
}
const testTimer = { timeout: 0 }
const testDoll = {
  getObject () { return testObj },
  getCel () { return testCel },
  getSound (soundFile) { return { play () { return `sound ${soundFile} played` } } },
  timers: [testTimer],
  setTimer (alarmId, duration) { return `timer ${alarmId} set for ${duration} ms` },
  logger: new Logger('debug')
}

describe('makeAction', function () {
  describe('altmap', function () {
    it('should work on objects', function () {
      const doll = testDoll
      const action = { action: 'altmap', args: [1] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('altmapped')
    })
    it('should work on cels', function () {
      const doll = testDoll
      const action = { action: 'altmap', args: ['somecel.cel'] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('altmapped')
    })
  })

  describe('map', function () {
    it('should work for objects', function () {
      const doll = testDoll
      const action = { action: 'map', args: [1] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('mapped')
    })
    it('should work for cels', function () {
      const doll = testDoll
      const action = { action: 'map', args: ['somecel.cel'] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('mapped')
    })
  })

  describe('timer', function () {
    it('should take two integer arguments', function () {
      const doll = testDoll
      const action = { action: 'timer', args: [0, 100] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('timer 0 set for 100 ms')
    })
  })

  describe('sound', function () {
    it('should work for wav files', function () {
      const doll = testDoll
      const action = { action: 'sound', args: ['some-sound.wav'] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('sound some-sound.wav played')
    })

    it('should rename au files to wav', function () {
      const doll = testDoll
      const action = { action: 'sound', args: ['some-sound.au'] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('sound some-sound.wav played')
    })
  })

  describe('unmap', function () {
    it('should work for objects', function () {
      const doll = testDoll
      const action = { action: 'unmap', args: [1] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('unmapped')
    })

    it('should work for cels', function () {
      const doll = testDoll
      const action = { action: 'unmap', args: ['some.cel'] }

      const result = makeAction(action, doll)

      expect(result()).to.equal('unmapped')
    })
  })
})

describe('objOrCelArg', function () {
  it('returns a cel if given a cel name', function () {
    const doll = testDoll
    const arg = 'test.cel'

    const result = objOrCelArg(arg, doll)

    expect(result).to.equal(testCel)
  })

  it('returns an object if given an object number', function () {
    const doll = testDoll
    const arg = 1

    const result = objOrCelArg(arg, doll)

    expect(result).to.equal(testObj)
  })

  it('returns undefined if given nonsense', function () {
    const doll = testDoll
    const arg = 'apple'

    const result = objOrCelArg(arg, doll)

    expect(result).to.be.undefined
  })
})
