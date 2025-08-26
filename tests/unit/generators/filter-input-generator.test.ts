import { describe, it, expect, beforeEach } from 'bun:test'
import { UnifiedFilterInputGenerator } from '@generators/unified/unified-filter-input-generator'
import { TestFixtures, TestMockFactory, SpyOutputStrategy } from '../../helpers'

describe('UnifiedFilterInputGenerator', () => {
	let generator: UnifiedFilterInputGenerator
	let context: any
	let spyStrategy: SpyOutputStrategy

	beforeEach(() => {
		const baseContext = TestFixtures.createContext({
			generateFilters: true,
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
		generator = new UnifiedFilterInputGenerator(context)
	})

	describe('Initialization', () => {
		it('should initialize successfully', () => {
			expect(generator).toBeDefined()
		})

		it('should call createCommonFilterTypes during initialization', () => {
			generator.generate()

			expect(spyStrategy.getGeneratedTypeNames()).toContain('StringFilterInput')
			expect(spyStrategy.getGeneratedTypeNames()).toContain('NumericFilterInput')
			expect(spyStrategy.getGeneratedTypeNames()).toContain('BooleanFilterInput')
		})
	})

	describe('Filter Generation', () => {
		it('should generate filter inputs when enabled', () => {
			const result = generator.generate()

			expect(result).toBeDefined()
			expect(Array.isArray(result)).toBe(true)
			expect(result.length).toBeGreaterThan(0)
		})

		it('should not generate filters when disabled', () => {
			const disabledContext = TestFixtures.createContext({
				generateFilters: false,
				models: [TestFixtures.createDataModel('UserFilterInput', [TestFixtures.createField('name', 'String')])],
			})

			const disabledUnifiedContext = TestMockFactory.createUnifiedContext(disabledContext)
			const disabledGenerator = new UnifiedFilterInputGenerator(disabledUnifiedContext)

			const result = disabledGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(0)
		})

		it('should generate correct number of filter inputs', () => {
			const result = generator.generate()

			expect(result.length).toBeGreaterThanOrEqual(2)

			expect(result).toContain('UserFilterInput')
			expect(result).toContain('PostFilterInput')

			expect(result).toContain('StringFilterInput')
			expect(result).toContain('NumericFilterInput')
			expect(result).toContain('BooleanFilterInput')
			expect(result).toContain('DateTimeFilterInput')
		})

		it('should respect field visibility rules', () => {
			generator.generate()

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')
			expect(filterCalls.length).toBe(2)
		})
	})

	describe('Field Processing', () => {
		it('should handle string fields correctly', () => {
			generator.generate()

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')
			const userFilterCall = filterCalls.find((call) => call.args[0] === 'User')
			expect(userFilterCall).toBeDefined()

			const fields = userFilterCall!.args[1]
			const nameField = fields.find((f: any) => f.name === 'name')
			expect(nameField).toBeDefined()
			expect(nameField.type).toBe('StringFilterInput')
		})

		it('should handle integer fields correctly', () => {
			generator.generate()

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')
			const userFilterCall = filterCalls.find((call) => call.args[0] === 'User')

			const fields = userFilterCall!.args[1]
			const ageField = fields.find((f: any) => f.name === 'age')
			expect(ageField).toBeDefined()
			expect(ageField.type).toBe('NumericFilterInput')
		})

		it('should handle boolean fields correctly', () => {
			generator.generate()

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')
			const userFilterCall = filterCalls.find((call) => call.args[0] === 'User')

			const fields = userFilterCall!.args[1]
			const isActiveField = fields.find((f: any) => f.name === 'isActive')
			expect(isActiveField).toBeDefined()
			expect(isActiveField.type).toBe('BooleanFilterInput')
		})

		it('should handle DateTime fields correctly', () => {
			generator.generate()

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')
			const userFilterCall = filterCalls.find((call) => call.args[0] === 'User')

			const fields = userFilterCall!.args[1]
			const createdAtField = fields.find((f: any) => f.name === 'createdAt')
			expect(createdAtField).toBeDefined()
			expect(createdAtField.type).toBe('DateTimeFilterInput')
		})

		it('should exclude relation fields from filters', () => {
			generator.generate()

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')
			const userFilterCall = filterCalls.find((call) => call.args[0] === 'User')

			const fields = userFilterCall!.args[1]
			const postsField = fields.find((f: any) => f.name === 'posts')
			expect(postsField).toBeUndefined()
		})

		it('should handle optional fields correctly', () => {
			generator.generate()

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')
			const postFilterCall = filterCalls.find((call) => call.args[0] === 'Post')

			const fields = postFilterCall!.args[1]
			const contentField = fields.find((f: any) => f.name === 'content')
			expect(contentField).toBeDefined()
			expect(contentField.nullable).toBe(true)
		})
	})

	describe('Error Handling', () => {
		it('should handle empty models gracefully', () => {
			const emptyContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					generateFilters: true,
					models: [],
				}),
			)
			const emptyGenerator = new UnifiedFilterInputGenerator(emptyContext)

			const result = emptyGenerator.generate()
			expect(result).toBeDefined()

			expect(result.length).toBeGreaterThanOrEqual(0)
		})

		it('should handle models with no filterable fields gracefully', () => {
			const noFieldsContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					generateFilters: true,
					models: [TestFixtures.createDataModel('Empty', [])],
				}),
			)
			const noFieldsGenerator = new UnifiedFilterInputGenerator(noFieldsContext)

			expect(() => {
				const result = noFieldsGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})

		it('should create empty filter input for models with no filterable fields', () => {
			const noFilterFieldsContext = TestMockFactory.createSpyUnifiedContext(
				TestFixtures.createContext({
					generateFilters: true,
					models: [
						TestFixtures.createDataModel('NoFilters', [
							TestFixtures.createField('id', 'String', false, false, [TestFixtures.createAttribute('@id')]),
							TestFixtures.createField('name', 'String', false, false, [TestFixtures.createAttribute('@db.VarChar', [{ value: 255 }])]),
						]),
					],
				}),
			)
			const noFilterFieldsGenerator = new UnifiedFilterInputGenerator(noFilterFieldsContext)

			const result = noFilterFieldsGenerator.generate()

			expect(result).toBeDefined()
			expect(result).toContain('NoFiltersFilterInput')

			const emptyCalls = noFilterFieldsContext.spy.getCallsForMethod('createEmptyFilterInputType')
			expect(emptyCalls.length).toBe(1)
			expect(emptyCalls[0]?.args[0]).toBe('NoFilters')
		})

		it('should handle malformed field types gracefully', () => {
			const malformedContext = TestFixtures.createContext({
				generateFilters: true,
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
			const malformedGenerator = new UnifiedFilterInputGenerator(malformedUnifiedContext)

			expect(() => {
				const result = malformedGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})
	})

	describe('Type Naming', () => {
		it('should format filter type names correctly', () => {
			generator.generate()

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')
			expect(filterCalls.length).toBe(2)

			const typeNames = filterCalls.map((call) => call.args[0])
			expect(typeNames).toContain('User')
			expect(typeNames).toContain('Post')
		})

		it('should handle custom naming conventions', () => {
			const customContext = TestFixtures.createContext({
				generateFilters: true,
				typeNaming: 'camelCase',
				fieldNaming: 'snake_case',
				models: [TestFixtures.createDataModel('user_profile', [TestFixtures.createField('first_name', 'String')])],
			})

			const customUnifiedContext = TestMockFactory.createSpyUnifiedContext(customContext)
			const customGenerator = new UnifiedFilterInputGenerator(customUnifiedContext)

			customGenerator.generate()

			const filterCalls = customUnifiedContext.spy.getCallsForMethod('createFilterInputType')
			expect(filterCalls.length).toBe(1)

			const fields = filterCalls[0]!.args[1]
			expect(fields.length).toBeGreaterThan(0)
		})
	})

	describe('Filter Field Definitions', () => {
		it('should create proper field definitions with descriptions', () => {
			generator.generate()

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')
			const userFilterCall = filterCalls.find((call) => call.args[0] === 'User')

			const fields = userFilterCall!.args[1]
			fields.forEach((field: any) => {
				expect(field).toHaveProperty('name')
				expect(field).toHaveProperty('type')
				expect(field).toHaveProperty('nullable')
				expect(field).toHaveProperty('description')
				expect(field.description).toContain('Filter by')
			})
		})

		it('should set all filter fields as nullable', () => {
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
		it('should return only filter input types', () => {
			const result = generator.generate()

			result.forEach((typeName) => {
				expect(typeof typeName).toBe('string')
				expect(typeName.length).toBeGreaterThan(0)
			})
		})

		it('should filter results by FilterInput suffix', () => {
			generator.generate()

			const allTypes = spyStrategy.getGeneratedTypeNames()
			const filterTypes = allTypes.filter((name) => name.endsWith('FilterInput'))

			expect(filterTypes.length).toBeGreaterThan(0)
		})

		it('should validate actual filter structure and field mapping', () => {
			const result = generator.generate()

			expect(result.length).toBeGreaterThanOrEqual(4)

			expect(result).toContain('UserFilterInput')
			expect(result).toContain('PostFilterInput')

			expect(result).toContain('StringFilterInput')
			expect(result).toContain('NumericFilterInput')
			expect(result).toContain('BooleanFilterInput')
			expect(result).toContain('DateTimeFilterInput')

			const filterCalls = spyStrategy.getCallsForMethod('createFilterInputType')
			expect(filterCalls.length).toBe(2)

			const userFilterCall = filterCalls.find((call) => call.args[0] === 'User')
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
		it('should handle models with mixed field types', () => {
			const mixedContext = TestFixtures.createContext({
				generateFilters: true,
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
			const mixedGenerator = new UnifiedFilterInputGenerator(mixedUnifiedContext)

			mixedGenerator.generate()

			const filterCalls = mixedUnifiedContext.spy.getCallsForMethod('createFilterInputType')
			expect(filterCalls.length).toBe(1)

			const fields = filterCalls[0]!.args[1]
			expect(fields.length).toBeGreaterThan(0)

			const relationField = fields.find((f: any) => f.name === 'relation')
			expect(relationField).toBeUndefined()
		})

		it('should handle deeply nested type definitions', () => {
			const nestedContext = TestFixtures.createContext({
				generateFilters: true,
				models: [
					TestFixtures.createDataModel('NestedModel', [
						TestFixtures.createField('level1', 'String'),
						TestFixtures.createField('level2', 'Int'),
						TestFixtures.createField('level3', 'Boolean'),
					]),
				],
			})

			const nestedUnifiedContext = TestMockFactory.createUnifiedContext(nestedContext)
			const nestedGenerator = new UnifiedFilterInputGenerator(nestedUnifiedContext)

			expect(() => {
				const result = nestedGenerator.generate()
				expect(result).toBeDefined()
				expect(result.length).toBeGreaterThan(0)
			}).not.toThrow()
		})
	})
})
