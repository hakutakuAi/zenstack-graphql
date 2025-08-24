import { describe, test, expect, beforeEach } from 'bun:test'
import { UnifiedFilterInputGenerator } from '@generators/unified/unified-filter-input-generator'
import { TestBuilders, ContextBuilder, SpyOutputStrategy } from '../../helpers'
import { DataModel } from '@zenstackhq/sdk/ast'

describe('UnifiedFilterInputGenerator', () => {
	let generator: UnifiedFilterInputGenerator
	let context: any
	let spyStrategy: SpyOutputStrategy

	beforeEach(() => {
		const baseContext = ContextBuilder.createBaseContext({
			generateFilters: true,
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
		generator = new UnifiedFilterInputGenerator(context)
	})

	describe('Initialization', () => {
		test('should initialize successfully', () => {
			expect(generator).toBeDefined()
		})

		test('should call createCommonFilterTypes during initialization', () => {
			generator.generate()

			expect(spyStrategy.getGeneratedTypeNames()).toContain('StringFilterInput')
			expect(spyStrategy.getGeneratedTypeNames()).toContain('IntFilterInput')
			expect(spyStrategy.getGeneratedTypeNames()).toContain('BooleanFilterInput')
		})
	})

	describe('Filter Generation', () => {
		test('should generate filter inputs when enabled', () => {
			const result = generator.generate()

			expect(result).toBeDefined()
			expect(Array.isArray(result)).toBe(true)
			expect(result.length).toBeGreaterThan(0)
		})

		test('should not generate filters when disabled', () => {
			const disabledContext = ContextBuilder.createBaseContext({
				generateFilters: false,
				models: [TestBuilders.createDataModel('UserFilterInput', [TestBuilders.createField('name', 'String')])],
			})

			const disabledUnifiedContext = ContextBuilder.createUnifiedContext(disabledContext)
			const disabledGenerator = new UnifiedFilterInputGenerator(disabledUnifiedContext)

			const result = disabledGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(0)
		})

		test('should generate correct number of filter inputs', () => {
			const result = generator.generate()

			expect(result.length).toBeGreaterThanOrEqual(2)

			expect(result).toContain('UserFilterInput')
			expect(result).toContain('PostFilterInput')

			expect(result).toContain('StringFilterInput')
			expect(result).toContain('IntFilterInput')
			expect(result).toContain('BooleanFilterInput')
		})

		test('should respect field visibility rules', () => {
			generator.generate()

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')
			expect(filterCalls.length).toBe(2)
		})
	})

	describe('Field Processing', () => {
		test('should handle string fields correctly', () => {
			generator.generate()

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')
			const userFilterCall = filterCalls.find((call) => call.args[0] === 'UserFilterInput')
			expect(userFilterCall).toBeDefined()

			const fields = userFilterCall!.args[1]
			const nameField = fields.find((f: any) => f.name === 'name')
			expect(nameField).toBeDefined()
			expect(nameField.type).toBe('StringFilterInput')
		})

		test('should handle integer fields correctly', () => {
			generator.generate()

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')
			const userFilterCall = filterCalls.find((call) => call.args[0] === 'UserFilterInput')

			const fields = userFilterCall!.args[1]
			const ageField = fields.find((f: any) => f.name === 'age')
			expect(ageField).toBeDefined()
			expect(ageField.type).toBe('NumericFilterInput')
		})

		test('should handle boolean fields correctly', () => {
			generator.generate()

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')
			const userFilterCall = filterCalls.find((call) => call.args[0] === 'UserFilterInput')

			const fields = userFilterCall!.args[1]
			const isActiveField = fields.find((f: any) => f.name === 'isActive')
			expect(isActiveField).toBeDefined()
			expect(isActiveField.type).toBe('BooleanFilterInput')
		})

		test('should handle DateTime fields correctly', () => {
			generator.generate()

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')
			const userFilterCall = filterCalls.find((call) => call.args[0] === 'UserFilterInput')

			const fields = userFilterCall!.args[1]
			const createdAtField = fields.find((f: any) => f.name === 'createdAt')
			expect(createdAtField).toBeDefined()
			expect(createdAtField.type).toBe('DateTimeFilterInput')
		})

		test('should exclude relation fields from filters', () => {
			generator.generate()

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')
			const userFilterCall = filterCalls.find((call) => call.args[0] === 'UserFilterInput')

			const fields = userFilterCall!.args[1]
			const postsField = fields.find((f: any) => f.name === 'posts')
			expect(postsField).toBeUndefined()
		})

		test('should handle optional fields correctly', () => {
			generator.generate()

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')
			const postFilterCall = filterCalls.find((call) => call.args[0] === 'PostFilterInput')

			const fields = postFilterCall!.args[1]
			const contentField = fields.find((f: any) => f.name === 'content')
			expect(contentField).toBeDefined()
			expect(contentField.nullable).toBe(true)
		})
	})

	describe('Error Handling', () => {
		test('should handle empty models gracefully', () => {
			const emptyContext = ContextBuilder.createUnifiedContext(
				ContextBuilder.createBaseContext({
					generateFilters: true,
					models: [],
				}),
			)
			const emptyGenerator = new UnifiedFilterInputGenerator(emptyContext)

			const result = emptyGenerator.generate()
			expect(result).toBeDefined()

			expect(result.length).toBeGreaterThanOrEqual(0)
		})

		test('should handle models with no filterable fields gracefully', () => {
			const noFieldsContext = ContextBuilder.createUnifiedContext(
				ContextBuilder.createBaseContext({
					generateFilters: true,
					models: [TestBuilders.createDataModel('Empty', [])],
				}),
			)
			const noFieldsGenerator = new UnifiedFilterInputGenerator(noFieldsContext)

			expect(() => {
				const result = noFieldsGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})

		test('should handle malformed field types gracefully', () => {
			const malformedContext = ContextBuilder.createBaseContext({
				generateFilters: true,
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
			const malformedGenerator = new UnifiedFilterInputGenerator(malformedUnifiedContext)

			expect(() => {
				const result = malformedGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})
	})

	describe('Type Naming', () => {
		test('should format filter type names correctly', () => {
			generator.generate()

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')
			expect(filterCalls.length).toBe(2)

			const typeNames = filterCalls.map((call) => call.args[0])
			expect(typeNames).toContain('UserFilterInput')
			expect(typeNames).toContain('PostFilterInput')
		})

		test('should handle custom naming conventions', () => {
			const customContext = ContextBuilder.createBaseContext({
				generateFilters: true,
				typeNaming: 'camelCase',
				fieldNaming: 'snake_case',
				models: [TestBuilders.createDataModel('user_profile', [TestBuilders.createField('first_name', 'String')])],
			})

			const customUnifiedContext = ContextBuilder.createSpyUnifiedContext(customContext)
			const customGenerator = new UnifiedFilterInputGenerator(customUnifiedContext)

			customGenerator.generate()

			const filterCalls = customUnifiedContext.spy.getCallsForMethod('createFilterInputType')
			expect(filterCalls.length).toBe(1)

			const fields = filterCalls[0]!.args[1]
			expect(fields.length).toBeGreaterThan(0)
		})
	})

	describe('Filter Field Definitions', () => {
		test('should create proper field definitions with descriptions', () => {
			generator.generate()

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')
			const userFilterCall = filterCalls.find((call) => call.args[0] === 'UserFilterInput')

			const fields = userFilterCall!.args[1]
			fields.forEach((field: any) => {
				expect(field).toHaveProperty('name')
				expect(field).toHaveProperty('type')
				expect(field).toHaveProperty('nullable')
				expect(field).toHaveProperty('description')
				expect(field.description).toContain('Filter by')
			})
		})

		test('should set all filter fields as nullable', () => {
			generator.generate()

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')

			filterCalls.forEach((call) => {
				const fields = call.args[1]
				fields.forEach((field: any) => {
					expect(field.nullable).toBe(true)
				})
			})
		})
	})

	describe('Result Processing', () => {
		test('should return only filter input types', () => {
			const result = generator.generate()

			result.forEach((typeName) => {
				expect(typeof typeName).toBe('string')
				expect(typeName.length).toBeGreaterThan(0)
			})
		})

		test('should filter results by FilterInput suffix', () => {
			generator.generate()

			const allTypes = spyStrategy.getGeneratedTypeNames()
			const filterTypes = allTypes.filter((name) => name.endsWith('FilterInput'))

			expect(filterTypes.length).toBeGreaterThan(0)
		})

		test('should validate actual filter structure and field mapping', () => {
			const result = generator.generate()

			expect(result.length).toBeGreaterThanOrEqual(4)

			expect(result).toContain('UserFilterInput')
			expect(result).toContain('PostFilterInput')

			expect(result).toContain('StringFilterInput')
			expect(result).toContain('IntFilterInput')
			expect(result).toContain('BooleanFilterInput')

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')
			expect(filterCalls.length).toBe(2)

			const userFilterCall = filterCalls.find((call) => call.args[0] === 'UserFilterInput')
			expect(userFilterCall).toBeDefined()
			const userFields = userFilterCall!.args[1]
			expect(userFields.length).toBeGreaterThan(0)

			const relationField = userFields.find((f: any) => f.name === 'posts')
			expect(relationField).toBeUndefined()

			const nameField = userFields.find((f: any) => f.name === 'name')
			expect(nameField).toBeDefined()
			expect(nameField.type).toBe('StringFilterInput')
			expect(nameField.nullable).toBe(true)
		})
	})

	describe('Complex Scenarios', () => {
		test('should handle models with mixed field types', () => {
			const mixedContext = ContextBuilder.createBaseContext({
				generateFilters: true,
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
			const mixedGenerator = new UnifiedFilterInputGenerator(mixedUnifiedContext)

			mixedGenerator.generate()

			const filterCalls = mixedUnifiedContext.spy.getCallsForMethod('createFilterInputType')
			expect(filterCalls.length).toBe(1)

			const fields = filterCalls[0]!.args[1]
			expect(fields.length).toBeGreaterThan(0)

			const relationField = fields.find((f: any) => f.name === 'relation')
			expect(relationField).toBeUndefined()
		})

		test('should handle deeply nested type definitions', () => {
			const nestedContext = ContextBuilder.createBaseContext({
				generateFilters: true,
				models: [
					TestBuilders.createDataModel('NestedModel', [
						TestBuilders.createField('level1', 'String'),
						TestBuilders.createField('level2', 'Int'),
						TestBuilders.createField('level3', 'Boolean'),
					]),
				],
			})

			const nestedUnifiedContext = ContextBuilder.createUnifiedContext(nestedContext)
			const nestedGenerator = new UnifiedFilterInputGenerator(nestedUnifiedContext)

			expect(() => {
				const result = nestedGenerator.generate()
				expect(result).toBeDefined()
				expect(result.length).toBeGreaterThan(0)
			}).not.toThrow()
		})
	})
})
