import { describe, test, expect, beforeEach } from 'bun:test'
import { UnifiedQueryArgsGenerator } from '@generators/unified/unified-query-args-generator'
import { TestFixtures, TestMockFactory, SpyOutputStrategy } from '../../helpers'

describe('UnifiedQueryArgsGenerator', () => {
	let generator: UnifiedQueryArgsGenerator
	let context: any
	let spyStrategy: SpyOutputStrategy

	beforeEach(() => {
		const baseContext = TestFixtures.createContext({
			models: [
				TestFixtures.createDataModel('User', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('name', 'String'),
					TestFixtures.createField('email', 'String'),
					TestFixtures.createField('age', 'Int'),
					TestFixtures.createField('isActive', 'Boolean'),
					TestFixtures.createField('createdAt', 'DateTime'),
				]),
				TestFixtures.createDataModel('Post', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('title', 'String'),
					TestFixtures.createField('content', 'String', true),
					TestFixtures.createField('published', 'Boolean'),
					TestFixtures.createField('viewCount', 'Int'),
				]),
			],
		})

		const spyContext = TestMockFactory.createSpyUnifiedContext(baseContext)
		context = spyContext
		spyStrategy = spyContext.spy
		generator = new UnifiedQueryArgsGenerator(context)
	})

	describe('Initialization', () => {
		test('should initialize successfully', () => {
			expect(generator).toBeDefined()
		})
	})

	describe('Query Args Generation', () => {
		test('should generate query args types for models', () => {
			const result = generator.generate()

			expect(result).toBeDefined()
			expect(Array.isArray(result)).toBe(true)
			expect(result.length).toBeGreaterThan(0)
		})

		test('should generate correct number of query args types', () => {
			const result = generator.generate()

			expect(result.length).toBe(2)
			expect(result).toContain('UserQueryArgs')
			expect(result).toContain('PostQueryArgs')
		})

		test('should create query args with correct parameters', () => {
			generator.generate()

			const queryArgsCalls = spyStrategy.getCallsForMethod('createQueryArgsInputType')
			expect(queryArgsCalls.length).toBe(2)

			const userQueryArgsCall = queryArgsCalls.find((call) => call.args[0] === 'UserQueryArgs')
			expect(userQueryArgsCall).toBeDefined()
			expect(userQueryArgsCall!.args[1].name).toBe('User')
			expect(userQueryArgsCall!.args[2]).toContain('Query arguments for User')

			const postQueryArgsCall = queryArgsCalls.find((call) => call.args[0] === 'PostQueryArgs')
			expect(postQueryArgsCall).toBeDefined()
			expect(postQueryArgsCall!.args[1].name).toBe('Post')
			expect(postQueryArgsCall!.args[2]).toContain('Query arguments for Post')
		})

		test('should include descriptions for query args types', () => {
			generator.generate()

			const queryArgsCalls = spyStrategy.getCallsForMethod('createQueryArgsInputType')

			queryArgsCalls.forEach((call) => {
				const description = call.args[2]
				expect(description).toBeDefined()
				expect(typeof description).toBe('string')
				expect(description.length).toBeGreaterThan(0)
				expect(description).toContain('filter, sort, and pagination')
			})
		})
	})

	describe('Type Naming', () => {
		test('should format query args type names correctly', () => {
			const result = generator.generate()

			expect(result).toContain('UserQueryArgs')
			expect(result).toContain('PostQueryArgs')

			result.forEach((typeName) => {
				expect(typeName).toMatch(/^[A-Z][a-zA-Z0-9]*QueryArgs$/)
			})
		})

		test('should handle custom naming conventions', () => {
			const customContext = TestFixtures.createContext({
				typeNaming: 'camelCase',
				fieldNaming: 'snake_case',
				models: [TestFixtures.createDataModel('user_profile', [TestFixtures.createField('first_name', 'String')])],
			})

			const customUnifiedContext = TestMockFactory.createSpyUnifiedContext(customContext)
			const customGenerator = new UnifiedQueryArgsGenerator(customUnifiedContext)

			const result = customGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(1)
			expect(result).toContain('userProfileQueryArgs')
		})
	})

	describe('Error Handling', () => {
		test('should handle empty models gracefully', () => {
			const emptyContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					models: [],
				}),
			)
			const emptyGenerator = new UnifiedQueryArgsGenerator(emptyContext)

			const result = emptyGenerator.generate()
			expect(result).toBeDefined()
			expect(result.length).toBe(0)
		})

		test('should handle models with no fields gracefully', () => {
			const noFieldsContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					models: [TestFixtures.createDataModel('Empty', [])],
				}),
			)
			const noFieldsGenerator = new UnifiedQueryArgsGenerator(noFieldsContext)

			expect(() => {
				const result = noFieldsGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})

		test('should handle generation errors gracefully', () => {
			const malformedModel = {
				$type: 'DataModel',
				name: null,
				fields: [],
				attributes: [],
				isAbstract: false,
				$container: undefined,
				comments: [],
				isView: false,
				superTypes: [],
			}

			const errorContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					models: [malformedModel as any],
				}),
			)
			const errorGenerator = new UnifiedQueryArgsGenerator(errorContext)

			expect(() => {
				const result = errorGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})
	})

	describe('Result Processing', () => {
		test('should return consistent result structure', () => {
			const result = generator.generate()

			expect(Array.isArray(result)).toBe(true)
			result.forEach((item) => {
				expect(typeof item).toBe('string')
				expect(item.length).toBeGreaterThan(0)
			})
		})

		test('should validate actual query args structure', () => {
			const result = generator.generate()

			expect(result.length).toBe(2)
			expect(result).toContain('UserQueryArgs')
			expect(result).toContain('PostQueryArgs')

			const queryArgsCalls = spyStrategy.getCallsForMethod('createQueryArgsInputType')
			expect(queryArgsCalls.length).toBe(2)

			const userQueryArgsCall = queryArgsCalls.find((call) => call.args[0] === 'UserQueryArgs')
			expect(userQueryArgsCall).toBeDefined()
			expect(userQueryArgsCall!.args[1]).toBeDefined()
			expect(userQueryArgsCall!.args[1].name).toBe('User')
		})
	})

	describe('Complex Scenarios', () => {
		test('should handle models with relations', () => {
			const relationContext = TestFixtures.createContext({
				includeRelations: true,
				models: [
					TestFixtures.createDataModel('User', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('name', 'String'),
						TestFixtures.createRelationField('posts', 'Post', false, true),
					]),
					TestFixtures.createDataModel('Post', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('title', 'String'),
						TestFixtures.createRelationField('author', 'User'),
					]),
				],
			})

			const relationUnifiedContext = TestMockFactory.createSpyUnifiedContext(relationContext)
			const relationGenerator = new UnifiedQueryArgsGenerator(relationUnifiedContext)

			const result = relationGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(2)
			expect(result).toContain('UserQueryArgs')
			expect(result).toContain('PostQueryArgs')

			const queryArgsCalls = relationUnifiedContext.spy.getCallsForMethod('createQueryArgsInputType')
			expect(queryArgsCalls.length).toBe(2)
		})

		test('should handle single model correctly', () => {
			const singleContext = TestFixtures.createContext({
				models: [TestFixtures.createDataModel('Single', [TestFixtures.createField('name', 'String')])],
			})

			const singleUnifiedContext = TestMockFactory.createUnifiedContext(singleContext)
			const singleGenerator = new UnifiedQueryArgsGenerator(singleUnifiedContext)

			const result = singleGenerator.generate()
			expect(result).toBeDefined()
			expect(result.length).toBe(1)
			expect(result).toContain('SingleQueryArgs')
		})

		test('should handle models with mixed field types', () => {
			const mixedContext = TestFixtures.createContext({
				models: [
					TestFixtures.createDataModel('ComplexModel', [
						TestFixtures.createField('stringField', 'String'),
						TestFixtures.createField('intField', 'Int'),
						TestFixtures.createField('floatField', 'Float'),
						TestFixtures.createField('boolField', 'Boolean'),
						TestFixtures.createField('dateField', 'DateTime'),
						TestFixtures.createField('optionalField', 'String', true),
					]),
				],
			})

			const mixedUnifiedContext = TestMockFactory.createSpyUnifiedContext(mixedContext)
			const mixedGenerator = new UnifiedQueryArgsGenerator(mixedUnifiedContext)

			const result = mixedGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(1)
			expect(result).toContain('ComplexModelQueryArgs')

			const queryArgsCalls = mixedUnifiedContext.spy.getCallsForMethod('createQueryArgsInputType')
			expect(queryArgsCalls.length).toBe(1)

			const complexQueryArgsCall = queryArgsCalls[0]!
			expect(complexQueryArgsCall.args[0]).toBe('ComplexModelQueryArgs')
			expect(complexQueryArgsCall.args[1].fields.length).toBe(6)
		})

		test('should handle models with different configurations', () => {
			const multiContext = TestFixtures.createContext({
				generateFilters: true,
				generateSorts: true,
				connectionTypes: true,
				models: [
					TestFixtures.createDataModel('Filterable', [
						TestFixtures.createField('searchField', 'String'),
						TestFixtures.createField('numberField', 'Int'),
					]),
					TestFixtures.createDataModel('Sortable', [
						TestFixtures.createField('sortField', 'String'),
						TestFixtures.createField('dateField', 'DateTime'),
					]),
				],
			})

			const multiUnifiedContext = TestMockFactory.createSpyUnifiedContext(multiContext)
			const multiGenerator = new UnifiedQueryArgsGenerator(multiUnifiedContext)

			const result = multiGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(2)
			expect(result).toContain('FilterableQueryArgs')
			expect(result).toContain('SortableQueryArgs')

			const queryArgsCalls = multiUnifiedContext.spy.getCallsForMethod('createQueryArgsInputType')
			expect(queryArgsCalls.length).toBe(2)
		})
	})

	describe('Integration with Filter and Sort', () => {
		test('should generate query args that support filtering and sorting', () => {
			const fullContext = TestFixtures.createContext({
				generateFilters: true,
				generateSorts: true,
				connectionTypes: true,
				models: [
					TestFixtures.createDataModel('Product', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('name', 'String'),
						TestFixtures.createField('price', 'Float'),
						TestFixtures.createField('inStock', 'Boolean'),
						TestFixtures.createField('createdAt', 'DateTime'),
					]),
				],
			})

			const fullUnifiedContext = TestMockFactory.createSpyUnifiedContext(fullContext)
			const fullGenerator = new UnifiedQueryArgsGenerator(fullUnifiedContext)

			const result = fullGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(1)
			expect(result).toContain('ProductQueryArgs')

			const queryArgsCalls = fullUnifiedContext.spy.getCallsForMethod('createQueryArgsInputType')
			expect(queryArgsCalls.length).toBe(1)

			const productQueryArgsCall = queryArgsCalls[0]!
			expect(productQueryArgsCall.args[0]).toBe('ProductQueryArgs')
			expect(productQueryArgsCall.args[2]).toContain('filter, sort, and pagination')
		})
	})
})
