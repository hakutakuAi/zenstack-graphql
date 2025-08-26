import { describe, it, expect, beforeEach } from 'bun:test'
import { TestMockFactory, SpyOutputStrategy, MockOutputStrategy } from '../../helpers'
import { TestFixtures } from '../../helpers'

describe('TestMockFactory', () => {
	describe('createUnifiedContext', () => {
		it('should create unified context from base context', () => {
			const baseContext = TestFixtures.createContext({
				models: [TestFixtures.createUserModel()],
				enums: [TestFixtures.createUserRoleEnum()],
			})

			const unifiedContext = TestMockFactory.createUnifiedContext(baseContext)

			expect(unifiedContext).toBeDefined()
			expect(unifiedContext.options).toBe(baseContext.options)
			expect(unifiedContext.models).toBe(baseContext.models)
			expect(unifiedContext.enums).toBe(baseContext.enums)
		})

		it('should create unified context with type mapper', () => {
			const baseContext = TestFixtures.createContext()
			const unifiedContext = TestMockFactory.createUnifiedContext(baseContext)

			expect(unifiedContext.typeMapper).toBeDefined()
		})

		it('should create unified context with output strategy', () => {
			const baseContext = TestFixtures.createContext()
			const unifiedContext = TestMockFactory.createUnifiedContext(baseContext)

			expect(unifiedContext.outputStrategy).toBeDefined()
			expect(typeof unifiedContext.outputStrategy.createObjectType).toBe('function')
		})
	})

	describe('createSpyUnifiedContext', () => {
		it('should create spy context with tracking capabilities', () => {
			const baseContext = TestFixtures.createContext()
			const spyContext = TestMockFactory.createSpyUnifiedContext(baseContext)

			expect(spyContext).toBeDefined()
			expect(spyContext.spy).toBeInstanceOf(SpyOutputStrategy)
			expect(spyContext.outputStrategy).toBe(spyContext.spy)
		})

		it('should track method calls', () => {
			const baseContext = TestFixtures.createContext()
			const spyContext = TestMockFactory.createSpyUnifiedContext(baseContext)

			spyContext.outputStrategy.createObjectType('User', {})

			const calls = spyContext.spy.getCallsForMethod('createObjectType')
			expect(calls).toHaveLength(1)
			expect(calls[0]?.method).toBe('createObjectType')
			expect(calls[0]?.args[0]).toBe('User')
		})

		it('should track generated type names', () => {
			const baseContext = TestFixtures.createContext()
			const spyContext = TestMockFactory.createSpyUnifiedContext(baseContext)

			spyContext.outputStrategy.createObjectType('User', {})
			const enumType = TestFixtures.createEnum('Status', ['ACTIVE'])
			spyContext.outputStrategy.createEnumType(enumType)

			const typeNames = spyContext.spy.getGeneratedTypeNames()
			expect(typeNames).toContain('User')
			expect(typeNames).toContain('Status')
		})
	})

	describe('createGraphQLContext', () => {
		it('should create GraphQL context', () => {
			const baseContext = TestFixtures.createContext()
			const graphqlContext = TestMockFactory.createGraphQLContext(baseContext)

			expect(graphqlContext).toBeDefined()
			expect(graphqlContext.registry).toBeDefined()
			expect(graphqlContext.typeMapper).toBeDefined()
			expect(graphqlContext.schemaComposer).toBeDefined()
		})
	})
})

describe('SpyOutputStrategy', () => {
	let spy: SpyOutputStrategy

	beforeEach(() => {
		spy = new SpyOutputStrategy()
	})

	describe('Method Tracking', () => {
		it('should track createObjectType calls', () => {
			spy.createObjectType('User', {})

			const calls = spy.getCallsForMethod('createObjectType')
			expect(calls).toHaveLength(1)
			expect(calls[0]?.args[0]).toBe('User')
		})

		it('should track createConnectionType calls', () => {
			spy.createConnectionType('User')

			const calls = spy.getCallsForMethod('createConnectionType')
			expect(calls).toHaveLength(1)
			expect(calls[0]?.args[0]).toBe('User')
		})

		it('should track createFilterInputType calls', () => {
			spy.createFilterInputType('User', [])

			const calls = spy.getCallsForMethod('createFilterInputType')
			expect(calls).toHaveLength(1)
			expect(calls[0]?.args[0]).toBe('User')
		})

		it('should track multiple method calls', () => {
			spy.createObjectType('User', {})
			spy.createConnectionType('User')
			spy.createObjectType('Post', {})

			const objectCalls = spy.getCallsForMethod('createObjectType')
			const connectionCalls = spy.getCallsForMethod('createConnectionType')

			expect(objectCalls).toHaveLength(2)
			expect(connectionCalls).toHaveLength(1)
		})
	})

	describe('Type Name Tracking', () => {
		it('should track generated type names', () => {
			spy.createObjectType('User', {})
			spy.createConnectionType('Post')
			const enumType = TestFixtures.createEnum('Status', ['ACTIVE'])
			spy.createEnumType(enumType)

			const typeNames = spy.getGeneratedTypeNames()
			expect(typeNames).toHaveLength(3)
			expect(typeNames).toContain('User')
			expect(typeNames).toContain('PostConnection')
			expect(typeNames).toContain('Status')
		})

		it('should track type names in order', () => {
			spy.createObjectType('User', {})
			spy.createObjectType('Post', {})
			spy.createConnectionType('User')

			const typeNames = spy.getGeneratedTypeNames()
			expect(typeNames[0]).toBe('User')
			expect(typeNames[1]).toBe('Post')
			expect(typeNames[2]).toBe('UserConnection')
		})
	})

	describe('Method Call Details', () => {
		it('should return empty array for non-existent method', () => {
			const calls = spy.getCallsForMethod('nonExistentMethod')
			expect(calls).toEqual([])
		})

		it('should track method call arguments', () => {
			spy.createObjectType('User', {
				id: { type: 'String!', description: 'User ID' },
				name: { type: 'String!', description: 'User name' },
			})

			const calls = spy.getCallsForMethod('createObjectType')
			expect(calls[0]?.args[1]).toEqual({
				id: { type: 'String!', description: 'User ID' },
				name: { type: 'String!', description: 'User name' },
			})
		})

		it('should track filter input creation', () => {
			const filterFields = [
				{ name: 'name', type: 'StringFilterInput', nullable: true },
				{ name: 'age', type: 'NumericFilterInput', nullable: true },
			]

			spy.createFilterInputType('User', filterFields)

			const calls = spy.getCallsForMethod('createFilterInputType')
			expect(calls[0]?.args[1]).toEqual(filterFields)
		})
	})
})

describe('MockOutputStrategy', () => {
	let mock: MockOutputStrategy

	beforeEach(() => {
		mock = new MockOutputStrategy()
	})

	describe('Method Implementations', () => {
		it('should implement createObjectType', () => {
			const result = mock.createObjectType('User', {})
			expect(result).toBeDefined()
			expect(result).toBe('User')
		})

		it('should implement createConnectionType', () => {
			const result = mock.createConnectionType('User')
			expect(result).toBeDefined()
			expect(result).toBe('UserConnection')
		})

		it('should implement createEnumType', () => {
			const enumType = TestFixtures.createEnum('Status', ['ACTIVE', 'INACTIVE'])
			const result = mock.createEnumType(enumType)
			expect(result).toBeDefined()
			expect(result).toBe('Status')
		})

		it('should implement all required methods', () => {
			expect(typeof mock.createObjectType).toBe('function')
			expect(typeof mock.createEnumType).toBe('function')
			expect(typeof mock.createConnectionType).toBe('function')
			expect(typeof mock.createFilterInputType).toBe('function')
			expect(typeof mock.createPaginationTypes).toBe('function')
			expect(typeof mock.createCommonFilterTypes).toBe('function')
		})
	})
})
