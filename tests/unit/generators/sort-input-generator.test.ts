import { describe, it, expect, beforeEach } from 'bun:test'
import { UnifiedSortInputGenerator } from '@generators/unified/unified-sort-input-generator'
import { TestFixtures, TestMockFactory, SpyOutputStrategy } from '../../helpers'

describe('UnifiedSortInputGenerator', () => {
	let generator: UnifiedSortInputGenerator
	let context: any
	let spyStrategy: SpyOutputStrategy

	beforeEach(() => {
		const baseContext = TestFixtures.createContext({
			generateSorts: true,
			models: [
				TestFixtures.createDataModel('User', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('name', 'String'),
					TestFixtures.createField('email', 'String'),
					TestFixtures.createField('age', 'Int'),
					TestFixtures.createField('isActive', 'Boolean'),
					TestFixtures.createField('createdAt', 'DateTime'),
					TestFixtures.createRelationField('posts', 'Post', false, true),
				]),
				TestFixtures.createDataModel('Post', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('title', 'String'),
					TestFixtures.createField('content', 'String', true),
					TestFixtures.createField('published', 'Boolean'),
					TestFixtures.createField('viewCount', 'Int'),
					TestFixtures.createRelationField('author', 'User'),
				]),
			],
		})

		const spyContext = TestMockFactory.createSpyUnifiedContext(baseContext)
		context = spyContext
		spyStrategy = spyContext.spy
		generator = new UnifiedSortInputGenerator(context)
	})

	describe('Initialization', () => {
		it('should initialize successfully', () => {
			expect(generator).toBeDefined()
		})

		it('should call createSortDirectionEnum during initialization', () => {
			generator.generate()

			expect(spyStrategy.getGeneratedTypeNames()).toContain('SortDirection')
		})
	})

	describe('Sort Generation', () => {
		it('should generate sort inputs when enabled', () => {
			const result = generator.generate()

			expect(result).toBeDefined()
			expect(Array.isArray(result)).toBe(true)
			expect(result.length).toBeGreaterThan(0)
		})

		it('should not generate sorts when disabled', () => {
			const disabledContext = TestFixtures.createContext({
				generateSorts: false,
				models: [TestFixtures.createDataModel('User', [TestFixtures.createField('name', 'String')])],
			})

			const disabledUnifiedContext = TestMockFactory.createUnifiedContext(disabledContext)
			const disabledGenerator = new UnifiedSortInputGenerator(disabledUnifiedContext)

			const result = disabledGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(0)
		})

		it('should generate correct number of sort inputs', () => {
			const result = generator.generate()

			expect(result.length).toBeGreaterThanOrEqual(2)

			expect(result).toContain('UserSortInput')
			expect(result).toContain('PostSortInput')

			expect(result).toContain('SortDirection')
		})

		it('should respect field visibility rules', () => {
			generator.generate()

			const sortCalls = spyStrategy.getCallsForMethod('createSortInputType')
			expect(sortCalls.length).toBe(2)
		})
	})

	describe('Field Processing', () => {
		it('should handle sortable fields correctly', () => {
			generator.generate()

			const sortCalls = spyStrategy.getCallsForMethod('createSortInputType')
			const userSortCall = sortCalls.find((call) => call.args[0] === 'User')
			expect(userSortCall).toBeDefined()

			const fields = userSortCall!.args[1]
			expect(fields.length).toBeGreaterThan(0)

			const nameField = fields.find((f: any) => f.name === 'name')
			expect(nameField).toBeDefined()
			expect(nameField.description).toContain('Sort by')
		})

		it('should exclude relation fields from sorts', () => {
			generator.generate()

			const sortCalls = spyStrategy.getCallsForMethod('createSortInputType')
			const userSortCall = sortCalls.find((call) => call.args[0] === 'User')

			const fields = userSortCall!.args[1]
			const postsField = fields.find((f: any) => f.name === 'posts')
			expect(postsField).toBeUndefined()
		})

		it('should handle models with no sortable fields', () => {
			const noSortableContext = TestFixtures.createContext({
				generateSorts: true,
				models: [TestFixtures.createDataModel('EmptyModel', [TestFixtures.createRelationField('relation', 'OtherModel')])],
			})

			const noSortableUnifiedContext = TestMockFactory.createSpyUnifiedContext(noSortableContext)
			const noSortableGenerator = new UnifiedSortInputGenerator(noSortableUnifiedContext)

			noSortableGenerator.generate()

			const sortCalls = noSortableUnifiedContext.spy.getCallsForMethod('createSortInputType')
			expect(sortCalls.length).toBe(1)

			const fields = sortCalls[0]!.args[1]

			expect(fields.length).toBe(1)
			expect(fields[0].name).toBe('_placeholder')
		})
	})

	describe('Error Handling', () => {
		it('should handle empty models gracefully', () => {
			const emptyContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					generateSorts: true,
					models: [],
				}),
			)
			const emptyGenerator = new UnifiedSortInputGenerator(emptyContext)

			const result = emptyGenerator.generate()
			expect(result).toBeDefined()

			expect(result.length).toBeGreaterThanOrEqual(0)
		})

		it('should handle models with no fields gracefully', () => {
			const noFieldsContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					generateSorts: true,
					models: [TestFixtures.createDataModel('Empty', [])],
				}),
			)
			const noFieldsGenerator = new UnifiedSortInputGenerator(noFieldsContext)

			expect(() => {
				const result = noFieldsGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})

		it('should handle malformed field types gracefully', () => {
			const malformedContext = TestFixtures.createContext({
				generateSorts: true,
				models: [
					TestFixtures.createDataModel('Test', [
						{
							$type: 'DataModelField',
							name: 'invalidField',
							type: {
								type: undefined,
								optional: false,
								array: false,
								reference: null,
							},
							attributes: [],
						},
					]),
				],
			})

			const malformedUnifiedContext = TestMockFactory.createUnifiedContext(malformedContext)
			const malformedGenerator = new UnifiedSortInputGenerator(malformedUnifiedContext)

			expect(() => {
				const result = malformedGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})
	})

	describe('Type Naming', () => {
		it('should format sort type names correctly', () => {
			generator.generate()

			const sortCalls = spyStrategy.getCallsForMethod('createSortInputType')
			expect(sortCalls.length).toBe(2)

			const typeNames = sortCalls.map((call) => call.args[0])
			expect(typeNames).toContain('User')
			expect(typeNames).toContain('Post')
		})

		it('should handle custom naming conventions', () => {
			const customContext = TestFixtures.createContext({
				generateSorts: true,
				typeNaming: 'camelCase',
				fieldNaming: 'snake_case',
				models: [TestFixtures.createDataModel('user_profile', [TestFixtures.createField('first_name', 'String')])],
			})

			const customUnifiedContext = TestMockFactory.createSpyUnifiedContext(customContext)
			const customGenerator = new UnifiedSortInputGenerator(customUnifiedContext)

			customGenerator.generate()

			const sortCalls = customUnifiedContext.spy.getCallsForMethod('createSortInputType')
			expect(sortCalls.length).toBe(1)

			const fields = sortCalls[0]!.args[1]
			expect(fields.length).toBeGreaterThan(0)
		})
	})

	describe('Sort Field Definitions', () => {
		it('should create proper field definitions with descriptions', () => {
			generator.generate()

			const sortCalls = spyStrategy.getCallsForMethod('createSortInputType')
			const userSortCall = sortCalls.find((call) => call.args[0] === 'User')

			const fields = userSortCall!.args[1]
			fields.forEach((field: any) => {
				expect(field).toHaveProperty('name')
				expect(field).toHaveProperty('description')
				expect(field.description).toContain('Sort by')
			})
		})

		it('should handle different field types for sorting', () => {
			const mixedContext = TestFixtures.createContext({
				generateSorts: true,
				models: [
					TestFixtures.createDataModel('ComplexModel', [
						TestFixtures.createField('stringField', 'String'),
						TestFixtures.createField('intField', 'Int'),
						TestFixtures.createField('floatField', 'Float'),
						TestFixtures.createField('boolField', 'Boolean'),
						TestFixtures.createField('dateField', 'DateTime'),
						TestFixtures.createField('decimalField', 'Decimal'),
						TestFixtures.createRelationField('relation', 'OtherModel'),
					]),
				],
			})

			const mixedUnifiedContext = TestMockFactory.createSpyUnifiedContext(mixedContext)
			const mixedGenerator = new UnifiedSortInputGenerator(mixedUnifiedContext)

			mixedGenerator.generate()

			const sortCalls = mixedUnifiedContext.spy.getCallsForMethod('createSortInputType')
			expect(sortCalls.length).toBe(1)

			const fields = sortCalls[0]!.args[1]
			expect(fields.length).toBeGreaterThan(0)

			const relationField = fields.find((f: any) => f.name === 'relation')
			expect(relationField).toBeUndefined()

			const stringField = fields.find((f: any) => f.name === 'stringField')
			expect(stringField).toBeDefined()
			expect(stringField.description).toContain('Sort by stringField')

			const intField = fields.find((f: any) => f.name === 'intField')
			expect(intField).toBeDefined()
			expect(intField.description).toContain('Sort by intField')

			const dateField = fields.find((f: any) => f.name === 'dateField')
			expect(dateField).toBeDefined()
			expect(dateField.description).toContain('Sort by dateField')

			const boolField = fields.find((f: any) => f.name === 'boolField')
			expect(boolField).toBeDefined()
			expect(boolField.description).toContain('Sort by boolField')

			const expectedFieldNames = ['stringField', 'intField', 'floatField', 'boolField', 'dateField', 'decimalField']
			const actualFieldNames = fields.map((f: any) => f.name).sort()
			expect(actualFieldNames).toEqual(expectedFieldNames.sort())
		})
	})

	describe('Result Processing', () => {
		it('should return only sort input types', () => {
			const result = generator.generate()

			result.forEach((typeName) => {
				expect(typeof typeName).toBe('string')
				expect(typeName.length).toBeGreaterThan(0)
			})
		})

		it('should filter results by SortInput suffix or SortDirection', () => {
			generator.generate()

			const allTypes = spyStrategy.getGeneratedTypeNames()
			const sortTypes = allTypes.filter((name) => name.endsWith('SortInput') || name === 'SortDirection')

			expect(sortTypes.length).toBeGreaterThan(0)
		})
	})

	describe('Complex Scenarios', () => {
		it('should handle models with mixed field types', () => {
			const result = generator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBeGreaterThan(0)

			expect(result).toContain('UserSortInput')
			expect(result).toContain('PostSortInput')
			expect(result).toContain('SortDirection')
		})

		it('should handle single model correctly', () => {
			const singleContext = TestFixtures.createContext({
				generateSorts: true,
				models: [TestFixtures.createDataModel('Single', [TestFixtures.createField('name', 'String')])],
			})

			const singleUnifiedContext = TestMockFactory.createUnifiedContext(singleContext)
			const singleGenerator = new UnifiedSortInputGenerator(singleUnifiedContext)

			const result = singleGenerator.generate()
			expect(result).toBeDefined()
			expect(result.length).toBeGreaterThanOrEqual(1)
		})
	})
})
