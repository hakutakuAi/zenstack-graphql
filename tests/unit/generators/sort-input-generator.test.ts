import { describe, test, expect, beforeEach } from 'bun:test'
import { UnifiedSortInputGenerator } from '@generators/unified/unified-sort-input-generator'
import { TestBuilders, ContextBuilder, SpyOutputStrategy } from '../../helpers'

describe('UnifiedSortInputGenerator', () => {
	let generator: UnifiedSortInputGenerator
	let context: any
	let spyStrategy: SpyOutputStrategy

	beforeEach(() => {
		const baseContext = ContextBuilder.createBaseContext({
			generateSorts: true,
			models: [
				TestBuilders.createDataModel('User', [
					TestBuilders.createField('id', 'String'),
					TestBuilders.createField('name', 'String'),
					TestBuilders.createField('email', 'String'),
					TestBuilders.createField('age', 'Int'),
					TestBuilders.createField('isActive', 'Boolean'),
					TestBuilders.createField('createdAt', 'DateTime'),
					TestBuilders.createRelationField('posts', 'Post', false, true),
				]),
				TestBuilders.createDataModel('Post', [
					TestBuilders.createField('id', 'String'),
					TestBuilders.createField('title', 'String'),
					TestBuilders.createField('content', 'String', true),
					TestBuilders.createField('published', 'Boolean'),
					TestBuilders.createField('viewCount', 'Int'),
					TestBuilders.createRelationField('author', 'User'),
				]),
			],
		})

		const spyContext = ContextBuilder.createSpyUnifiedContext(baseContext)
		context = spyContext
		spyStrategy = spyContext.spy
		generator = new UnifiedSortInputGenerator(context)
	})

	describe('Initialization', () => {
		test('should initialize successfully', () => {
			expect(generator).toBeDefined()
		})

		test('should call createSortDirectionEnum during initialization', () => {
			generator.generate()

			expect(spyStrategy.getGeneratedTypeNames()).toContain('SortDirection')
		})
	})

	describe('Sort Generation', () => {
		test('should generate sort inputs when enabled', () => {
			const result = generator.generate()

			expect(result).toBeDefined()
			expect(Array.isArray(result)).toBe(true)
			expect(result.length).toBeGreaterThan(0)
		})

		test('should not generate sorts when disabled', () => {
			const disabledContext = ContextBuilder.createBaseContext({
				generateSorts: false,
				models: [TestBuilders.createDataModel('User', [TestBuilders.createField('name', 'String')])],
			})

			const disabledUnifiedContext = ContextBuilder.createUnifiedContext(disabledContext)
			const disabledGenerator = new UnifiedSortInputGenerator(disabledUnifiedContext)

			const result = disabledGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(0)
		})

		test('should generate correct number of sort inputs', () => {
			const result = generator.generate()

			expect(result.length).toBeGreaterThanOrEqual(2)

			expect(result).toContain('UserSortInput')
			expect(result).toContain('PostSortInput')

			expect(result).toContain('SortDirection')
		})

		test('should respect field visibility rules', () => {
			generator.generate()

			const sortCalls = spyStrategy.getCallsForMethod('createSortInputType')
			expect(sortCalls.length).toBe(2)
		})
	})

	describe('Field Processing', () => {
		test('should handle sortable fields correctly', () => {
			generator.generate()

			const sortCalls = spyStrategy.getCallsForMethod('createSortInputType')
			const userSortCall = sortCalls.find((call) => call.args[0] === 'UserSortInput')
			expect(userSortCall).toBeDefined()

			const fields = userSortCall!.args[1]
			expect(fields.length).toBeGreaterThan(0)

			const nameField = fields.find((f: any) => f.name === 'name')
			expect(nameField).toBeDefined()
			expect(nameField.description).toContain('Sort by')
		})

		test('should exclude relation fields from sorts', () => {
			generator.generate()

			const sortCalls = spyStrategy.getCallsForMethod('createSortInputType')
			const userSortCall = sortCalls.find((call) => call.args[0] === 'UserSortInput')

			const fields = userSortCall!.args[1]
			const postsField = fields.find((f: any) => f.name === 'posts')
			expect(postsField).toBeUndefined()
		})

		test('should handle models with no sortable fields', () => {
			const noSortableContext = ContextBuilder.createBaseContext({
				generateSorts: true,
				models: [TestBuilders.createDataModel('EmptyModel', [TestBuilders.createRelationField('relation', 'OtherModel')])],
			})

			const noSortableUnifiedContext = ContextBuilder.createSpyUnifiedContext(noSortableContext)
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
		test('should handle empty models gracefully', () => {
			const emptyContext = ContextBuilder.createUnifiedContext(
				ContextBuilder.createBaseContext({
					generateSorts: true,
					models: [],
				}),
			)
			const emptyGenerator = new UnifiedSortInputGenerator(emptyContext)

			const result = emptyGenerator.generate()
			expect(result).toBeDefined()

			expect(result.length).toBeGreaterThanOrEqual(0)
		})

		test('should handle models with no fields gracefully', () => {
			const noFieldsContext = ContextBuilder.createUnifiedContext(
				ContextBuilder.createBaseContext({
					generateSorts: true,
					models: [TestBuilders.createDataModel('Empty', [])],
				}),
			)
			const noFieldsGenerator = new UnifiedSortInputGenerator(noFieldsContext)

			expect(() => {
				const result = noFieldsGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})

		test('should handle malformed field types gracefully', () => {
			const malformedContext = ContextBuilder.createBaseContext({
				generateSorts: true,
				models: [
					TestBuilders.createDataModel('Test', [
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

			const malformedUnifiedContext = ContextBuilder.createUnifiedContext(malformedContext)
			const malformedGenerator = new UnifiedSortInputGenerator(malformedUnifiedContext)

			expect(() => {
				const result = malformedGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})
	})

	describe('Type Naming', () => {
		test('should format sort type names correctly', () => {
			generator.generate()

			const sortCalls = spyStrategy.getCallsForMethod('createSortInputType')
			expect(sortCalls.length).toBe(2)

			const typeNames = sortCalls.map((call) => call.args[0])
			expect(typeNames).toContain('UserSortInput')
			expect(typeNames).toContain('PostSortInput')
		})

		test('should handle custom naming conventions', () => {
			const customContext = ContextBuilder.createBaseContext({
				generateSorts: true,
				typeNaming: 'camelCase',
				fieldNaming: 'snake_case',
				models: [TestBuilders.createDataModel('user_profile', [TestBuilders.createField('first_name', 'String')])],
			})

			const customUnifiedContext = ContextBuilder.createSpyUnifiedContext(customContext)
			const customGenerator = new UnifiedSortInputGenerator(customUnifiedContext)

			customGenerator.generate()

			const sortCalls = customUnifiedContext.spy.getCallsForMethod('createSortInputType')
			expect(sortCalls.length).toBe(1)

			const fields = sortCalls[0]!.args[1]
			expect(fields.length).toBeGreaterThan(0)
		})
	})

	describe('Sort Field Definitions', () => {
		test('should create proper field definitions with descriptions', () => {
			generator.generate()

			const sortCalls = spyStrategy.getCallsForMethod('createSortInputType')
			const userSortCall = sortCalls.find((call) => call.args[0] === 'UserSortInput')

			const fields = userSortCall!.args[1]
			fields.forEach((field: any) => {
				expect(field).toHaveProperty('name')
				expect(field).toHaveProperty('description')
				expect(field.description).toContain('Sort by')
			})
		})

		test('should handle different field types for sorting', () => {
			const mixedContext = ContextBuilder.createBaseContext({
				generateSorts: true,
				models: [
					TestBuilders.createDataModel('ComplexModel', [
						TestBuilders.createField('stringField', 'String'),
						TestBuilders.createField('intField', 'Int'),
						TestBuilders.createField('floatField', 'Float'),
						TestBuilders.createField('boolField', 'Boolean'),
						TestBuilders.createField('dateField', 'DateTime'),
						TestBuilders.createField('decimalField', 'Decimal'),
						TestBuilders.createRelationField('relation', 'OtherModel'),
					]),
				],
			})

			const mixedUnifiedContext = ContextBuilder.createSpyUnifiedContext(mixedContext)
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
		})
	})

	describe('Result Processing', () => {
		test('should return only sort input types', () => {
			const result = generator.generate()

			result.forEach((typeName) => {
				expect(typeof typeName).toBe('string')
				expect(typeName.length).toBeGreaterThan(0)
			})
		})

		test('should filter results by SortInput suffix or SortDirection', () => {
			generator.generate()

			const allTypes = spyStrategy.getGeneratedTypeNames()
			const sortTypes = allTypes.filter((name) => name.endsWith('SortInput') || name === 'SortDirection')

			expect(sortTypes.length).toBeGreaterThan(0)
		})
	})

	describe('Complex Scenarios', () => {
		test('should handle models with mixed field types', () => {
			const result = generator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBeGreaterThan(0)

			expect(result).toContain('UserSortInput')
			expect(result).toContain('PostSortInput')
			expect(result).toContain('SortDirection')
		})

		test('should handle single model correctly', () => {
			const singleContext = ContextBuilder.createBaseContext({
				generateSorts: true,
				models: [TestBuilders.createDataModel('Single', [TestBuilders.createField('name', 'String')])],
			})

			const singleUnifiedContext = ContextBuilder.createUnifiedContext(singleContext)
			const singleGenerator = new UnifiedSortInputGenerator(singleUnifiedContext)

			const result = singleGenerator.generate()
			expect(result).toBeDefined()
			expect(result.length).toBeGreaterThanOrEqual(1)
		})
	})
})
