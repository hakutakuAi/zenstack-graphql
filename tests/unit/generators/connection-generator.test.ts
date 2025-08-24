import { describe, test, expect, beforeEach } from 'bun:test'
import { UnifiedConnectionGenerator } from '@generators/unified/unified-connection-generator'
import { TestFixtures, TestMockFactory, SpyOutputStrategy } from '../../helpers'

describe('UnifiedConnectionGenerator', () => {
	let generator: UnifiedConnectionGenerator
	let context: any
	let spyStrategy: SpyOutputStrategy

	beforeEach(() => {
		const baseContext = TestFixtures.createContext({
			connectionTypes: true,
			models: [
				TestFixtures.createDataModel('User', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('name', 'String'),
					TestFixtures.createField('email', 'String'),
				]),
				TestFixtures.createDataModel('Post', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('title', 'String'),
					TestFixtures.createField('content', 'String'),
				]),
			],
		})

		const spyContext = TestMockFactory.createSpyUnifiedContext(baseContext)
		context = spyContext
		spyStrategy = spyContext.spy
		generator = new UnifiedConnectionGenerator(context)
	})

	describe('Initialization', () => {
		test('should initialize successfully', () => {
			expect(generator).toBeDefined()
		})

		test('should call createPaginationTypes during initialization', () => {
			generator.generate()

			expect(spyStrategy.getGeneratedTypeNames()).toContain('PageInfo')
			expect(spyStrategy.getGeneratedTypeNames()).toContain('PaginationInput')
		})
	})

	describe('Connection Generation', () => {
		test('should generate connection types when enabled', () => {
			const result = generator.generate()

			expect(result).toBeDefined()
			expect(Array.isArray(result)).toBe(true)
			expect(result.length).toBeGreaterThan(0)
		})

		test('should not generate connections when disabled', () => {
			const disabledContext = TestFixtures.createContext({
				connectionTypes: false,
				models: [TestFixtures.createDataModel('User', [TestFixtures.createField('name', 'String')])],
			})

			const disabledUnifiedContext = TestMockFactory.createUnifiedContext(disabledContext)
			const disabledGenerator = new UnifiedConnectionGenerator(disabledUnifiedContext)

			const result = disabledGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(0)
		})

		test('should generate correct number of connection types', () => {
			const result = generator.generate()

			expect(result.length).toBe(2)
			expect(result).toContain('UserConnection')
			expect(result).toContain('PostConnection')
		})

		test('should create connection types for each model', () => {
			generator.generate()

			const connectionCalls = spyStrategy.getCallsForMethod('createConnectionType')
			expect(connectionCalls.length).toBe(2)

			const typeNames = connectionCalls.map((call) => call.args[0])
			expect(typeNames).toContain('User')
			expect(typeNames).toContain('Post')
		})
	})

	describe('Type Naming', () => {
		test('should format connection type names correctly', () => {
			const result = generator.generate()

			expect(result).toContain('UserConnection')
			expect(result).toContain('PostConnection')
		})

		test('should handle custom naming conventions', () => {
			const customContext = TestFixtures.createContext({
				connectionTypes: true,
				typeNaming: 'camelCase',
				models: [TestFixtures.createDataModel('user_profile', [TestFixtures.createField('first_name', 'String')])],
			})

			const customUnifiedContext = TestMockFactory.createSpyUnifiedContext(customContext)
			const customGenerator = new UnifiedConnectionGenerator(customUnifiedContext)

			const result = customGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(1)
		})
	})

	describe('Error Handling', () => {
		test('should handle empty models gracefully', () => {
			const emptyContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					connectionTypes: true,
					models: [],
				}),
			)
			const emptyGenerator = new UnifiedConnectionGenerator(emptyContext)

			const result = emptyGenerator.generate()
			expect(result).toBeDefined()
			expect(result.length).toBe(0)
		})

		test('should handle models with no fields gracefully', () => {
			const noFieldsContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					connectionTypes: true,
					models: [TestFixtures.createDataModel('Empty', [])],
				}),
			)
			const noFieldsGenerator = new UnifiedConnectionGenerator(noFieldsContext)

			expect(() => {
				const result = noFieldsGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})
	})

	describe('Result Processing', () => {
		test('should return only connection type names', () => {
			const result = generator.generate()

			result.forEach((typeName) => {
				expect(typeof typeName).toBe('string')
				expect(typeName.length).toBeGreaterThan(0)
				expect(typeName.endsWith('Connection')).toBe(true)
			})
		})

		test('should filter results by Connection suffix', () => {
			generator.generate()

			const allTypes = spyStrategy.getGeneratedTypeNames()
			const connectionTypes = allTypes.filter((name) => name.endsWith('Connection'))

			expect(connectionTypes.length).toBe(2)
		})

		test('should validate actual connection structure', () => {
			const result = generator.generate()

			expect(result.length).toBe(2)
			expect(result).toContain('UserConnection')
			expect(result).toContain('PostConnection')

			const connectionCalls = spyStrategy.getCallsForMethod('createConnectionType')
			expect(connectionCalls.length).toBe(2)

			const userConnectionCall = connectionCalls.find((call) => call.args[0] === 'User')
			expect(userConnectionCall).toBeDefined()

			const postConnectionCall = connectionCalls.find((call) => call.args[0] === 'Post')
			expect(postConnectionCall).toBeDefined()
		})
	})

	describe('Complex Scenarios', () => {
		test('should handle models with relations', () => {
			const relationContext = TestFixtures.createContext({
				connectionTypes: true,
				models: [
					TestFixtures.createDataModel('User', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createRelationField('posts', 'Post', false, true),
					]),
					TestFixtures.createDataModel('Post', [TestFixtures.createField('id', 'String'), TestFixtures.createRelationField('author', 'User')]),
				],
			})

			const relationUnifiedContext = TestMockFactory.createUnifiedContext(relationContext)
			const relationGenerator = new UnifiedConnectionGenerator(relationUnifiedContext)

			const result = relationGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(2)
			expect(result).toContain('UserConnection')
			expect(result).toContain('PostConnection')
		})

		test('should handle single model correctly', () => {
			const singleContext = TestFixtures.createContext({
				connectionTypes: true,
				models: [TestFixtures.createDataModel('Single', [TestFixtures.createField('name', 'String')])],
			})

			const singleUnifiedContext = TestMockFactory.createUnifiedContext(singleContext)
			const singleGenerator = new UnifiedConnectionGenerator(singleUnifiedContext)

			const result = singleGenerator.generate()
			expect(result).toBeDefined()
			expect(result.length).toBe(1)
			expect(result).toContain('SingleConnection')
		})
	})

	describe('Pagination Integration', () => {
		test('should create pagination types before generating connections', () => {
			generator.generate()

			const paginationCalls = spyStrategy.getCallsForMethod('createPaginationTypes')
			expect(paginationCalls).toBeDefined()

			expect(spyStrategy.getGeneratedTypeNames()).toContain('PageInfo')
			expect(spyStrategy.getGeneratedTypeNames()).toContain('PaginationInput')
		})

		test('should generate PageInfo and PaginationInput types', () => {
			const result = generator.generate()

			const allTypes = spyStrategy.getGeneratedTypeNames()
			expect(allTypes).toContain('PageInfo')
			expect(allTypes).toContain('PaginationInput')
			expect(allTypes).toContain('UserConnection')
			expect(allTypes).toContain('PostConnection')
		})
	})
})
