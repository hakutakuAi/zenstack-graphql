import { describe, it, expect, beforeEach } from 'bun:test'
import { SchemaProcessor } from '@utils/schema/schema-processor'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { TestFixtures, SchemaBuilder } from '../../helpers'
import { DataModel } from '@zenstackhq/sdk/ast'

describe('Schema Processor', () => {
	let processor: SchemaProcessor
	let typeFormatter: TypeFormatter
	let model: DataModel

	beforeEach(() => {
		processor = new SchemaProcessor()
		typeFormatter = TypeFormatter.fromOptions('PascalCase', 'camelCase')

		model = TestFixtures.createDataModel('User', [
			{
				...TestFixtures.createField('id', 'String'),
				attributes: [],
			},
			{
				...TestFixtures.createField('name', 'String'),
				attributes: [TestFixtures.createAttribute('@graphql.name', [{ value: 'fullName' }])],
			},
			{
				...TestFixtures.createField('email', 'String'),
				attributes: [TestFixtures.createAttribute('@graphql.ignore')],
			},
			{
				...TestFixtures.createField('age', 'Int'),
				attributes: [TestFixtures.createAttribute('@graphql.sortable')],
			},
			{
				...TestFixtures.createField('bio', 'String'),
				attributes: [TestFixtures.createAttribute('@graphql.filterable')],
			},
			{
				...TestFixtures.createField('score', 'Float'),
				attributes: [TestFixtures.createAttribute('@graphql.sortable'), TestFixtures.createAttribute('@graphql.filterable')],
			},
		])

		model.attributes = [
			TestFixtures.createAttribute('@@graphql.name', [{ value: 'UserType' }]),
			TestFixtures.createAttribute('@@graphql.description', [{ value: 'A user in the system' }]),
		]
	})

	describe('Static Factory Method', () => {
		it('should create processor from model', () => {
			const modelChain = SchemaProcessor.fromModel(model)

			expect(modelChain.name()).toBe('UserType')
			expect(modelChain.description()).toBe('A user in the system')
		})
	})

	describe('Model Processing', () => {
		it('should extract model name from attribute', () => {
			const modelChain = processor.model(model)

			expect(modelChain.name()).toBe('UserType')
		})

		it('should fall back to original model name when no attribute', () => {
			const simpleModel = TestFixtures.createDataModel('SimpleUser')
			const modelChain = processor.model(simpleModel)

			expect(modelChain.name()).toBe('SimpleUser')
		})

		it('should extract model description', () => {
			const modelChain = processor.model(model)

			expect(modelChain.description()).toBe('A user in the system')
		})

		it('should return undefined for missing description', () => {
			const simpleModel = TestFixtures.createDataModel('SimpleUser')
			const modelChain = processor.model(simpleModel)

			expect(modelChain.description()).toBeUndefined()
		})

		it('should detect ignored models', () => {
			const ignoredModel = TestFixtures.createDataModel('IgnoredUser')
			ignoredModel.attributes = [TestFixtures.createAttribute('@@graphql.ignore')]

			const modelChain = processor.model(ignoredModel)
			expect(modelChain.isIgnored()).toBe(true)
		})

		it('should detect non-ignored models', () => {
			const modelChain = processor.model(model)
			expect(modelChain.isIgnored()).toBe(false)
		})

		it('should format type name correctly', () => {
			const modelChain = processor.model(model)
			const formattedName = modelChain.getFormattedTypeName(typeFormatter)

			expect(formattedName).toBe('UserType')
		})

		it('should format original name when no attribute', () => {
			const simpleModel = TestFixtures.createDataModel('simpleUser')
			const modelChain = processor.model(simpleModel)
			const formattedName = modelChain.getFormattedTypeName(typeFormatter)

			expect(formattedName).toBe('SimpleUser')
		})
	})

	describe('Field Processing', () => {
		it('should extract field name from attribute', () => {
			const fieldChain = processor.field(model, 'name')

			expect(fieldChain.name()).toBe('fullName')
		})

		it('should fall back to original field name', () => {
			const fieldChain = processor.field(model, 'id')

			expect(fieldChain.name()).toBe('id')
		})

		it('should detect ignored fields', () => {
			const fieldChain = processor.field(model, 'email')

			expect(fieldChain.isIgnored()).toBe(true)
		})

		it('should detect non-ignored fields', () => {
			const fieldChain = processor.field(model, 'id')

			expect(fieldChain.isIgnored()).toBe(false)
		})

		it('should detect sortable fields', () => {
			const fieldChain = processor.field(model, 'age')

			expect(fieldChain.isSortable()).toBe(true)
		})

		it('should detect non-sortable fields', () => {
			const fieldChain = processor.field(model, 'id')

			expect(fieldChain.isSortable()).toBe(false)
		})

		it('should detect filterable fields', () => {
			const fieldChain = processor.field(model, 'bio')

			expect(fieldChain.isFilterable()).toBe(true)
		})

		it('should detect non-filterable fields', () => {
			const fieldChain = processor.field(model, 'id')

			expect(fieldChain.isFilterable()).toBe(false)
		})

		it('should detect fields with multiple attributes', () => {
			const fieldChain = processor.field(model, 'score')

			expect(fieldChain.isSortable()).toBe(true)
			expect(fieldChain.isFilterable()).toBe(true)
		})

		it('should check field existence', () => {
			const existingField = processor.field(model, 'id')
			const nonExistingField = processor.field(model, 'nonexistent')

			expect(existingField.exists()).toBe(true)
			expect(nonExistingField.exists()).toBe(false)
		})

		it('should format field names correctly', () => {
			const fieldChain = processor.field(model, 'name')
			const formattedName = fieldChain.getFormattedFieldName(typeFormatter)

			expect(formattedName).toBe('fullName')
		})

		it('should format original field name when no attribute', () => {
			const fieldChain = processor.field(model, 'user_id')
			const formattedName = fieldChain.getFormattedFieldName(typeFormatter)

			expect(formattedName).toBe('userId')
		})
	})

	describe('Field Type Analysis', () => {
		it('should identify sortable field types', () => {
			const testCases = [
				{ name: 'stringField', type: 'String', expected: true },
				{ name: 'intField', type: 'Int', expected: true },
				{ name: 'floatField', type: 'Float', expected: true },
				{ name: 'decimalField', type: 'Decimal', expected: true },
				{ name: 'dateTimeField', type: 'DateTime', expected: true },
				{ name: 'booleanField', type: 'Boolean', expected: true },
				{ name: 'jsonField', type: 'Json', expected: false },
				{ name: 'bytesField', type: 'Bytes', expected: false },
			]

			for (const testCase of testCases) {
				const testModel = TestFixtures.createDataModel('TestModel', [TestFixtures.createField(testCase.name, testCase.type)])

				const fieldChain = processor.field(testModel, testCase.name)
				expect(fieldChain.isSortableType()).toBe(testCase.expected)
			}
		})

		it('should identify range filterable field types', () => {
			const testCases = [
				{ name: 'stringField', type: 'String', expected: false },
				{ name: 'intField', type: 'Int', expected: true },
				{ name: 'floatField', type: 'Float', expected: true },
				{ name: 'decimalField', type: 'Decimal', expected: true },
				{ name: 'dateTimeField', type: 'DateTime', expected: true },
				{ name: 'booleanField', type: 'Boolean', expected: false },
			]

			for (const testCase of testCases) {
				const testModel = TestFixtures.createDataModel('TestModel', [TestFixtures.createField(testCase.name, testCase.type)])

				const fieldChain = processor.field(testModel, testCase.name)
				expect(fieldChain.isRangeFilterableType()).toBe(testCase.expected)
			}
		})

		it('should identify string searchable field types', () => {
			const testCases = [
				{ name: 'stringField', type: 'String', expected: true },
				{ name: 'intField', type: 'Int', expected: false },
				{ name: 'floatField', type: 'Float', expected: false },
			]

			for (const testCase of testCases) {
				const testModel = TestFixtures.createDataModel('TestModel', [TestFixtures.createField(testCase.name, testCase.type)])

				const fieldChain = processor.field(testModel, testCase.name)
				expect(fieldChain.isStringSearchableType()).toBe(testCase.expected)
			}
		})

		it('should handle missing field type gracefully', () => {
			const fieldChain = processor.field(model, 'nonexistent')

			expect(fieldChain.isSortableType()).toBe(false)
			expect(fieldChain.isRangeFilterableType()).toBe(false)
			expect(fieldChain.isStringSearchableType()).toBe(false)
		})
	})

	describe('Field Inclusion Logic', () => {
		it('should include regular fields', () => {
			const fieldChain = processor.field(model, 'id')

			expect(fieldChain.shouldInclude(false)).toBe(true)
		})

		it('should exclude ignored fields', () => {
			const fieldChain = processor.field(model, 'email')

			expect(fieldChain.shouldInclude(false)).toBe(false)
		})

		it('should include relations when requested', () => {
			const modelWithRelation = TestFixtures.createDataModel('User', [TestFixtures.createRelationField('posts', 'Post', false, true)])

			const fieldChain = processor.field(modelWithRelation, 'posts')

			expect(fieldChain.shouldInclude(true)).toBe(true)
		})

		it('should exclude relations when not requested', () => {
			const modelWithRelation = TestFixtures.createDataModel('User', [TestFixtures.createRelationField('posts', 'Post', false, true)])

			const fieldChain = processor.field(modelWithRelation, 'posts')

			expect(fieldChain.shouldInclude(false)).toBe(false)
		})

		it('should handle non-existent fields', () => {
			const fieldChain = processor.field(model, 'nonexistent')

			expect(fieldChain.shouldInclude(true)).toBe(false)
			expect(fieldChain.shouldInclude(false)).toBe(false)
		})
	})

	describe('Attribute Handling', () => {
		it('should find attributes by exact name', () => {
			const fieldChain = processor.field(model, 'name')

			expect(fieldChain.attr('@graphql.name')).toBe(true)
			expect(fieldChain.attr('@graphql.ignore')).toBe(false)
		})

		it('should extract string values from attributes', () => {
			const fieldChain = processor.field(model, 'name')

			expect(fieldChain.getString('name')).toBe('fullName')
		})

		it('should return undefined for missing string attributes', () => {
			const fieldChain = processor.field(model, 'id')

			expect(fieldChain.getString('name')).toBeUndefined()
		})

		it('should handle complex attribute structures', () => {
			const complexModel = TestFixtures.createDataModel('Complex', [
				{
					...TestFixtures.createField('priority', 'Int'),
					attributes: [
						TestFixtures.createAttribute('@graphql.sortable', [
							{ name: 'enabled', value: true },
							{ name: 'priority', value: 10 },
						]),
					],
				},
			])

			const fieldChain = processor.field(complexModel, 'priority')

			expect(fieldChain.getBoolean('enabled')).toBe(true)
			expect(fieldChain.getNumber('priority')).toBe(10)
		})
	})

	describe('Edge Cases', () => {
		it('should handle empty model', () => {
			const emptyModel = TestFixtures.createDataModel('Empty')
			const modelChain = processor.model(emptyModel)

			expect(modelChain.name()).toBe('Empty')
			expect(modelChain.description()).toBeUndefined()
			expect(modelChain.isIgnored()).toBe(false)
		})

		it('should handle model with no fields', () => {
			const emptyModel = TestFixtures.createDataModel('Empty')
			const fieldChain = processor.field(emptyModel, 'nonexistent')

			expect(fieldChain.exists()).toBe(false)
			expect(fieldChain.name()).toBe('nonexistent')
		})

		it('should handle attributes with no arguments', () => {
			const simpleModel = TestFixtures.createDataModel('Simple', [
				{
					...TestFixtures.createField('active', 'Boolean'),
					attributes: [TestFixtures.createAttribute('@graphql.sortable')],
				},
			])

			const fieldChain = processor.field(simpleModel, 'active')
			expect(fieldChain.isSortable()).toBe(true)
		})

		it('should handle malformed attributes gracefully', () => {
			const malformedModel = TestFixtures.createDataModel('Malformed', [
				{
					...TestFixtures.createField('broken', 'String'),
					attributes: [{ decl: null, args: [] }],
				},
			])

			const fieldChain = processor.field(malformedModel, 'broken')
			expect(fieldChain.getString('name')).toBeUndefined()
		})
	})
})
