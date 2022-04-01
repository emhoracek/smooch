/* eslint-disable no-unused-expressions */
/* globals describe, it, afterEach */
import { expect } from 'chai'
import { addEvent } from '../fkissEvent.mjs'
import { Logger } from '../logger.mjs'

const testObj = {
  addEventListener (eventName) { this.events.push(eventName) },
  events: []
}
const testCel = {
  addEventListener (eventName) { this.events.push(eventName) },
  events: []
}
const testDoll = {
  addEventListener (eventName) { this.events.push(eventName) },
  getObject () { return testObj },
  getCel () { return testCel },
  timers: [],
  events: [],
  logger: new Logger('debug')
}

describe('addEvent', function () {
  afterEach(function () {
    testDoll.timers = []
    testDoll.events = []
    testObj.events = []
    testCel.events = []
  })

  describe('alarm', function () {
    it('should set an alarm', function () {
      const doll = testDoll
      const event = { event: 'alarm', args: [1], actions: [] }

      addEvent(event, doll)

      expect(doll.timers[1]).not.to.be.undefined
    })
  })

  describe('begin', function () {
    it('should add an event to the doll', function () {
      const doll = testDoll
      const event = { event: 'begin', args: [], actions: [] }

      addEvent(event, doll)

      expect(doll.events[0]).to.equal('begin')
    })
  })

  describe('catch', function () {
    it('should add an event to the object', function () {
      const doll = testDoll
      const event = { event: 'catch', args: [1], actions: [] }

      addEvent(event, doll)

      expect(testObj.events[0]).to.equal('catch')
    })

    it('should add an event to the cel', function () {
      const doll = testDoll
      const event = { event: 'catch', args: ['somecel.cel'], actions: [] }

      addEvent(event, doll)

      expect(testCel.events[0]).to.equal('catch')
    })
  })

  describe('col', function () {
    it('should not be implemented', function () {
      const doll = testDoll
      const event = { event: 'col', args: [], actions: [] }

      const result = addEvent(event, doll)

      expect(result).to.be.undefined
    })
  })

  describe('drop', function () {
    it('should add an event to the object', function () {
      const doll = testDoll
      const event = { event: 'drop', args: [1], actions: [] }

      addEvent(event, doll)

      expect(testObj.events[0]).to.equal('drop')
    })

    it('should add an event to the cel', function () {
      const doll = testDoll
      const event = { event: 'drop', args: ['somecel.cel'], actions: [] }

      addEvent(event, doll)

      expect(testCel.events[0]).to.equal('drop')
    })
  })

  describe('end', function () {
    it('should not be implemented', function () {
      const doll = testDoll
      const event = { event: 'end', args: [], actions: [] }

      const result = addEvent(event, doll)

      expect(result).to.be.undefined
    })
  })

  describe('fixcatch', function () {
    it('should add an event to the object', function () {
      const doll = testDoll
      const event = { event: 'fixcatch', args: [1], actions: [] }

      addEvent(event, doll)

      expect(testObj.events[0]).to.equal('fixcatch')
    })

    it('should add an event to the cel', function () {
      const doll = testDoll
      const event = { event: 'fixcatch', args: ['somecel.cel'], actions: [] }

      addEvent(event, doll)

      expect(testCel.events[0]).to.equal('fixcatch')
    })
  })

  describe('fixdrop', function () {
    it('should add an event to the object', function () {
      const doll = testDoll
      const event = { event: 'fixdrop', args: [1], actions: [] }

      addEvent(event, doll)

      expect(testObj.events[0]).to.equal('fixdrop')
    })

    it('should add an event to the cel', function () {
      const doll = testDoll
      const event = { event: 'fixdrop', args: ['somecel.cel'], actions: [] }

      addEvent(event, doll)

      expect(testCel.events[0]).to.equal('fixdrop')
    })
  })

  describe('initialize', function () {
    it('should add an event to the doll', function () {
      const doll = testDoll
      const event = { event: 'initialize', args: [], actions: [] }

      addEvent(event, doll)

      expect(testDoll.events[0]).to.equal('initialize')
    })
  })

  describe('never', function () {
    it('should not be implemented', function () {
      const doll = testDoll
      const event = { event: 'fixdrop', args: [1], actions: [] }

      const result = addEvent(event, doll)

      expect(result).to.be.undefined
    })
  })

  describe('press', function () {
    it('should add an event to the object', function () {
      const doll = testDoll
      const event = { event: 'press', args: [1], actions: [] }

      addEvent(event, doll)

      expect(testObj.events[0]).to.equal('press')
    })

    it('should add an event to the cel', function () {
      const doll = testDoll
      const event = { event: 'press', args: ['somecel.cel'], actions: [] }

      addEvent(event, doll)

      expect(testCel.events[0]).to.equal('press')
    })
  })

  describe('release', function () {
    it('should add an event to the object', function () {
      const doll = testDoll
      const event = { event: 'release', args: [1], actions: [] }

      addEvent(event, doll)

      expect(testObj.events[0]).to.equal('release')
    })

    it('should add an event to the cel', function () {
      const doll = testDoll
      const event = { event: 'release', args: ['somecel.cel'], actions: [] }

      addEvent(event, doll)

      expect(testCel.events[0]).to.equal('release')
    })
  })

  describe('set', function () {
    it('should add an event to the doll', function () {
      const doll = testDoll
      const event = { event: 'set', args: [1], actions: [] }

      addEvent(event, doll)

      expect(testDoll.events[0]).to.equal('set-1')
    })
  })

  describe('unfix', function () {
    it('should not be implemented', function () {
      const doll = testDoll
      const event = { event: 'unfix', args: [1], actions: [] }

      const result = addEvent(event, doll)

      expect(result).to.be.undefined
    })
  })
})
