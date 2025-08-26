import { describe, it, expect, beforeEach } from 'bun:test'
import { GraphQLTypeFactories } from '@utils/schema/graphql-type-factories'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { TestFixtures } from '../../helpers'
import { SchemaComposer } from 'graphql-compose'
import { DataModel, Enum } from '@zenstackhq/sdk/ast'

describe('GraphQLTypeFactories', () => {
	let factories: GraphQLTypeFactories
	let schemaComposer: SchemaComposer
	let typeFormatter: TypeFormatter

	beforeEach(() => {
		schemaComposer = new SchemaComposer()
		typeFormatter = new TypeFormatter('PascalCase', 'camelCase')
		factories = new GraphQLTypeFactories(schemaComposer, typeFormatter)
	})

	describe('Initialization', () => {
		it('should initialize successfully', () => {
			expect(factories).toBeDefined()
		})
	})

	describe('Filter Input Type Creation', () => {
		it('should create filter input type for model with fields', () => {
			const model: DataModel = TestFixtures.createDataModel('User', [
				TestFixtures.createField('id', 'String'),
				TestFixtures.createField('name', 'String'),
				TestFixtures.createField('email', 'String'),
			])

			const result = factories.createFilterInputType(model, 'User')

			expect(result).toBeDefined()
			expect(result.getTypeName()).toBe('UserFilterInput')

			const fields = result.getFields()
			expect(fields.id).toBeDefined()
			expect(fields.name).toBeDefined()
			expect(fields.email).toBeDefined()
		})

		it('should create placeholder filter input for model with no filterable fields', () => {
			const model: DataModel = TestFixtures.createDataModel('EmptyModel', [])

			const result = factories.createFilterInputType(model, 'EmptyModel')

			expect(result).toBeDefined()
			expect(result.getTypeName()).toBe('EmptyModelFilterInput')

			const fields = result.getFields()
			expect(fields._placeholder).toBeDefined()
			expect(fields._placeholder?.type.getTypeName()).toBe('String')
		})

		it('should reuse existing filter input type', () => {
			const model: DataModel = TestFixtures.createDataModel('User', [TestFixtures.createField('id', 'String')])

			const result1 = factories.createFilterInputType(model, 'User')
			const result2 = factories.createFilterInputType(model, 'User')

			expect(result1).toBe(result2)
			expect(result1.getTypeName()).toBe('UserFilterInput')
		})

		it('should handle filter input creation errors gracefully', () => {
			const malformedModel = {
				...TestFixtures.createDataModel('ErrorModel', []),
				fields: null as any,
			}

			const result = factories.createFilterInputType(malformedModel, 'ErrorModel')

			expect(result).toBeDefined()
			expect(result.getTypeName()).toBe('ErrorModelFilterInput')

			const fields = result.getFields()
			expect(fields._placeholder).toBeDefined()
		})

		it('should create filter input with correct field descriptions', () => {
			const model: DataModel = TestFixtures.createDataModel('Product', [
				TestFixtures.createField('name', 'String'),
				TestFixtures.createField('price', 'Decimal'),
			])

			const result = factories.createFilterInputType(model, 'Product')

			const fields = result.getFields()
			expect(fields.name?.description).toBe('Filter by name')
			expect(fields.price?.description).toBe('Filter by price')
		})
	})

	describe('Input Type Creation', () => {
		it('should create input type for model with fields', () => {
			const model: DataModel = TestFixtures.createDataModel('CreateUser', [
				TestFixtures.createField('name', 'String'),
				TestFixtures.createField('email', 'String'),
				TestFixtures.createField('age', 'Int'),
			])

			const result = factories.createInputType(model, 'CreateUser')

			expect(result).toBeDefined()
			expect(result.getTypeName()).toBe('CreateUserInput')

			const fields = result.getFields()
			expect(fields.name).toBeDefined()
			expect(fields.email).toBeDefined()
			expect(fields.age).toBeDefined()
		})

		it('should create placeholder input type for model with no fields', () => {
			const model: DataModel = TestFixtures.createDataModel('EmptyInput', [])

			const result = factories.createInputType(model, 'EmptyInput')

			expect(result).toBeDefined()
			expect(result.getTypeName()).toBe('EmptyInputInput')

			const fields = result.getFields()
			expect(fields._placeholder).toBeDefined()
			expect(fields._placeholder?.description).toBe('Placeholder field when no input fields are available')
			expect(fields._placeholder?.type.getTypeName()).toBe('String')
		})

		it('should reuse existing input type', () => {
			const model: DataModel = TestFixtures.createDataModel('User', [TestFixtures.createField('name', 'String')])

			const result1 = factories.createInputType(model, 'User')
			const result2 = factories.createInputType(model, 'User')

			expect(result1).toBe(result2)
			expect(result1.getTypeName()).toBe('UserInput')
		})

		it('should handle input type creation errors gracefully', () => {
			const malformedModel = {
				...TestFixtures.createDataModel('ErrorInput', []),
				fields: undefined as any,
			}

			const result = factories.createInputType(malformedModel, 'ErrorInput')

			expect(result).toBeDefined()
			expect(result.getTypeName()).toBe('ErrorInputInput')

			const fields = result.getFields()
			expect(fields._placeholder).toBeDefined()
		})

		it('should create input type with correct field descriptions', () => {
			const model: DataModel = TestFixtures.createDataModel('UpdateProduct', [
				TestFixtures.createField('title', 'String'),
				TestFixtures.createField('description', 'String'),
			])

			const result = factories.createInputType(model, 'UpdateProduct')

			const fields = result.getFields()
			expect(fields.title?.description).toBe('Input for title')
			expect(fields.description?.description).toBe('Input for description')
		})
	})

	describe('Enum Type Creation', () => {
		it('should create enum type with values', () => {
			const enumType: Enum = TestFixtures.createEnum('Status', ['ACTIVE', 'INACTIVE', 'PENDING'])

			const result = factories.createEnumType(enumType)

			expect(result).toBeDefined()
			expect(result.getTypeName()).toBe('Status')

			const values = result.getFields()
			expect(values.ACTIVE).toBeDefined()
			expect(values.INACTIVE).toBeDefined()
			expect(values.PENDING).toBeDefined()

			expect(values.ACTIVE?.value).toBe('ACTIVE')
			expect(values.ACTIVE?.description).toBe('ACTIVE value')
		})

		it('should create enum type with custom naming', () => {
			const customFormatter = new TypeFormatter('camelCase', 'snake_case')
			const customFactories = new GraphQLTypeFactories(schemaComposer, customFormatter)

			const enumType: Enum = TestFixtures.createEnum('user_status', ['active', 'inactive'])

			const result = customFactories.createEnumType(enumType)

			expect(result).toBeDefined()
			expect(result.getTypeName()).toBe('userStatus')
		})

		it('should reuse existing enum type', () => {
			const enumType: Enum = TestFixtures.createEnum('Priority', ['HIGH', 'MEDIUM', 'LOW'])

			const result1 = factories.createEnumType(enumType)
			const result2 = factories.createEnumType(enumType)

			expect(result1).toBe(result2)
			expect(result1.getTypeName()).toBe('Priority')
		})

		it('should handle enum type creation errors gracefully', () => {
			const malformedEnum = {
				...TestFixtures.createEnum('ErrorEnum', ['VALUE1']),
				fields: null as any,
			}

			const result = factories.createEnumType(malformedEnum)

			expect(result).toBeDefined()
			expect(result.getTypeName()).toBe('ErrorEnum')

			const values = result.getFields()
			expect(values.UNKNOWN).toBeDefined()
			expect(values.UNKNOWN?.value).toBe('UNKNOWN')
		})

		it('should handle empty enum gracefully', () => {
			const emptyEnum: Enum = TestFixtures.createEnum('EmptyEnum', [])

			const result = factories.createEnumType(emptyEnum)

			expect(result).toBeDefined()
			expect(result.getTypeName()).toBe('EmptyEnum')
		})
	})

	describe('Complex Scenarios', () => {
		it('should handle multiple types creation in same composer', () => {
			const userModel: DataModel = TestFixtures.createDataModel('User', [
				TestFixtures.createField('id', 'String'),
				TestFixtures.createField('name', 'String'),
			])

			const statusEnum: Enum = TestFixtures.createEnum('Status', ['ACTIVE', 'INACTIVE'])

			const filterInput = factories.createFilterInputType(userModel, 'User')
			const createInput = factories.createInputType(userModel, 'User')
			const enumType = factories.createEnumType(statusEnum)

			expect(filterInput.getTypeName()).toBe('UserFilterInput')
			expect(createInput.getTypeName()).toBe('UserInput')
			expect(enumType.getTypeName()).toBe('Status')

			expect(schemaComposer.has('UserFilterInput')).toBe(true)
			expect(schemaComposer.has('UserInput')).toBe(true)
			expect(schemaComposer.has('Status')).toBe(true)
		})

		it('should handle complex field types in filter inputs', () => {
			const complexModel: DataModel = TestFixtures.createDataModel('ComplexModel', [
				TestFixtures.createField('stringField', 'String'),
				TestFixtures.createField('intField', 'Int'),
				TestFixtures.createField('boolField', 'Boolean'),
				TestFixtures.createField('dateField', 'DateTime'),
				TestFixtures.createField('jsonField', 'Json'),
				TestFixtures.createField('optionalField', 'String', true),
				TestFixtures.createField('arrayField', 'String', false, true),
			])

			const result = factories.createFilterInputType(complexModel, 'ComplexModel')

			expect(result).toBeDefined()
			const fields = result.getFields()
			expect(Object.keys(fields).length).toBeGreaterThan(5)
			expect(fields.stringField).toBeDefined()
			expect(fields.intField).toBeDefined()
			expect(fields.jsonField).toBeDefined()
			expect(fields.arrayField).toBeDefined()
		})

		it('should maintain type consistency across multiple calls', () => {
			const model: DataModel = TestFixtures.createDataModel('ConsistentModel', [TestFixtures.createField('name', 'String')])

			const filter1 = factories.createFilterInputType(model, 'ConsistentModel')
			const input1 = factories.createInputType(model, 'ConsistentModel')

			const filter2 = factories.createFilterInputType(model, 'ConsistentModel')
			const input2 = factories.createInputType(model, 'ConsistentModel')

			expect(filter1).toBe(filter2)
			expect(input1).toBe(input2)

			expect(filter1.getFields().name).toBeDefined()
			expect(input1.getFields().name).toBeDefined()
		})
	})

	describe('Error Recovery', () => {
		it('should recover from field iteration errors in filter inputs', () => {
			const modelWithBadField = {
				...TestFixtures.createDataModel('BadFieldModel', []),
				fields: [
					{
						name: 'goodField',
						type: { type: 'String' },
					},
					null,
					{
						name: 'anotherGoodField',
						type: { type: 'Int' },
					},
				] as any,
			}

			const result = factories.createFilterInputType(modelWithBadField, 'BadFieldModel')

			expect(result).toBeDefined()
			expect(result.getTypeName()).toBe('BadFieldModelFilterInput')
		})

		it('should recover from field iteration errors in input types', () => {
			const modelWithBadField = {
				...TestFixtures.createDataModel('BadInputModel', []),
				fields: [
					{
						name: 'validField',
						type: { type: 'String' },
					},
					undefined,
				] as any,
			}

			const result = factories.createInputType(modelWithBadField, 'BadInputModel')

			expect(result).toBeDefined()
			expect(result.getTypeName()).toBe('BadInputModelInput')
		})

		it('should recover from enum field iteration errors', () => {
			const enumWithBadField = {
				...TestFixtures.createEnum('BadEnum', []),
				fields: [{ name: 'VALID_VALUE' }, null, { name: 'ANOTHER_VALID_VALUE' }] as any,
			}

			const result = factories.createEnumType(enumWithBadField)

			expect(result).toBeDefined()
			expect(result.getTypeName()).toBe('BadEnum')
		})
	})
})
