import { describe, it, expect, beforeEach } from 'bun:test'
import { UnifiedTypeMapper, FieldTypeCategory } from '@utils/type-mapping/unified-type-mapper'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { TestFixtures } from '../../helpers'
import { DataModel, Enum } from '@zenstackhq/sdk/ast'
import { NormalizedOptions } from '@utils/config'
import { OutputFormat } from '@utils/constants'

describe('UnifiedTypeMapper', () => {
	let typeMapper: UnifiedTypeMapper
	let typeFormatter: TypeFormatter
	let models: DataModel[]
	let enums: Enum[]
	let options: NormalizedOptions

	const createTestField = (name: string, type: string, isOptional = false, isArray = false) => {
		return TestFixtures.createField(name, type, isOptional, isArray) as any
	}

	const createTestRelationField = (name: string, referencedModel: string, isOptional = false, isArray = false) => {
		return TestFixtures.createRelationField(name, referencedModel, isOptional, isArray) as any
	}

	beforeEach(() => {
		typeFormatter = new TypeFormatter('PascalCase', 'camelCase')
		models = [
			TestFixtures.createDataModel('User', [
				TestFixtures.createField('id', 'String'),
				TestFixtures.createField('name', 'String'),
				TestFixtures.createField('email', 'String'),
				TestFixtures.createRelationField('posts', 'Post', false, true),
			]),
			TestFixtures.createDataModel('Post', [
				TestFixtures.createField('id', 'String'),
				TestFixtures.createField('title', 'String'),
				TestFixtures.createRelationField('author', 'User'),
			]),
		]
		enums = [TestFixtures.createEnum('Status', ['ACTIVE', 'INACTIVE', 'PENDING']), TestFixtures.createEnum('Priority', ['HIGH', 'MEDIUM', 'LOW'])]
		options = TestFixtures.createOptions()
		typeMapper = new UnifiedTypeMapper(typeFormatter, models, enums, options)
	})

	describe('Initialization', () => {
		it('should initialize successfully', () => {
			expect(typeMapper).toBeDefined()
		})

		it('should initialize with empty models and enums', () => {
			const emptyTypeMapper = new UnifiedTypeMapper(typeFormatter, [], [], options)
			expect(emptyTypeMapper).toBeDefined()
		})
	})

	describe('Field Type Mapping', () => {
		it('should map string field correctly', () => {
			const field = createTestField('testField', 'String', false, false)
			const result = typeMapper.mapFieldType(field)
			expect(result).toBe('String!')
		})

		it('should map optional string field correctly', () => {
			const field = createTestField('testField', 'String', true, false)
			const result = typeMapper.mapFieldType(field)
			expect(result).toBe('String')
		})

		it('should map array string field correctly', () => {
			const field = createTestField('testField', 'String', false, true)
			const result = typeMapper.mapFieldType(field)
			expect(result).toBe('[String!]!')
		})

		it('should map optional array string field correctly', () => {
			const field = createTestField('testField', 'String', true, true)
			const result = typeMapper.mapFieldType(field)
			expect(result).toBe('[String]')
		})

		it('should map int field correctly', () => {
			const field = createTestField('testField', 'Int', false, false)
			const result = typeMapper.mapFieldType(field)
			expect(result).toBe('Int!')
		})

		it('should map boolean field correctly', () => {
			const field = createTestField('testField', 'Boolean', false, false)
			const result = typeMapper.mapFieldType(field)
			expect(result).toBe('Boolean!')
		})

		it('should map float field correctly', () => {
			const field = createTestField('testField', 'Float', false, false)
			const result = typeMapper.mapFieldType(field)
			expect(result).toBe('Float!')
		})

		it('should map DateTime field correctly', () => {
			const field = createTestField('testField', 'DateTime', false, false)
			const result = typeMapper.mapFieldType(field)
			expect(result).toBe('DateTime!')
		})

		it('should map Json field correctly', () => {
			const field = createTestField('testField', 'Json', false, false)
			const result = typeMapper.mapFieldType(field)
			expect(result).toBe('JSON!')
		})

		it('should map Decimal field correctly', () => {
			const field = createTestField('testField', 'Decimal', false, false)
			const result = typeMapper.mapFieldType(field)
			expect(result).toBe('Decimal!')
		})

		it('should map BigInt field correctly', () => {
			const field = createTestField('testField', 'BigInt', false, false)
			const result = typeMapper.mapFieldType(field)
			expect(result).toBe('Int!')
		})

		it('should map Bytes field correctly', () => {
			const field = createTestField('testField', 'Bytes', false, false)
			const result = typeMapper.mapFieldType(field)
			expect(result).toBe('String!')
		})
	})

	describe('TypeScript Format Mapping', () => {
		it('should map string field to TypeScript format', () => {
			const field = createTestField('testField', 'String', false, false)
			const result = typeMapper.mapFieldType(field, OutputFormat.TYPE_GRAPHQL)
			expect(result).toBe('string')
		})

		it('should map optional string field to TypeScript format', () => {
			const field = createTestField('testField', 'String', true, false)
			const result = typeMapper.mapFieldType(field, OutputFormat.TYPE_GRAPHQL)
			expect(result).toBe('string')
		})

		it('should map array string field to TypeScript format', () => {
			const field = createTestField('testField', 'String', false, true)
			const result = typeMapper.mapFieldType(field, OutputFormat.TYPE_GRAPHQL)
			expect(result).toBe('string[]')
		})

		it('should map int field to TypeScript format', () => {
			const field = createTestField('testField', 'Int', false, false)
			const result = typeMapper.mapFieldType(field, OutputFormat.TYPE_GRAPHQL)
			expect(result).toBe('number')
		})

		it('should map boolean field to TypeScript format', () => {
			const field = createTestField('testField', 'Boolean', false, false)
			const result = typeMapper.mapFieldType(field, OutputFormat.TYPE_GRAPHQL)
			expect(result).toBe('boolean')
		})

		it('should map DateTime field to TypeScript format', () => {
			const field = createTestField('testField', 'DateTime', false, false)
			const result = typeMapper.mapFieldType(field, OutputFormat.TYPE_GRAPHQL)
			expect(result).toBe('DateTime')
		})
	})

	describe('Relation Field Type Mapping', () => {
		it('should map single relation field correctly', () => {
			const field = createTestRelationField('user', 'User', false, false)
			const result = typeMapper.mapFieldType(field)
			expect(result).toBe('User!')
		})

		it('should map optional relation field correctly', () => {
			const field = createTestRelationField('user', 'User', true, false)
			const result = typeMapper.mapFieldType(field)
			expect(result).toBe('User')
		})

		it('should map array relation field correctly', () => {
			const field = createTestRelationField('posts', 'Post', false, true)
			const result = typeMapper.mapFieldType(field)
			expect(result).toBe('[Post!]!')
		})

		it('should map optional array relation field correctly', () => {
			const field = createTestRelationField('posts', 'Post', true, true)
			const result = typeMapper.mapFieldType(field)
			expect(result).toBe('[Post]')
		})
	})

	describe('Scalar Type Mapping', () => {
		it('should map scalar types correctly for GraphQL format', () => {
			expect(typeMapper.mapScalarType('String', OutputFormat.GRAPHQL)).toBe('String')
			expect(typeMapper.mapScalarType('Int', OutputFormat.GRAPHQL)).toBe('Int')
			expect(typeMapper.mapScalarType('Boolean', OutputFormat.GRAPHQL)).toBe('Boolean')
			expect(typeMapper.mapScalarType('Float', OutputFormat.GRAPHQL)).toBe('Float')
		})

		it('should map scalar types correctly for TypeScript format', () => {
			expect(typeMapper.mapScalarType('String', OutputFormat.TYPE_GRAPHQL)).toBe('string')
			expect(typeMapper.mapScalarType('Int', OutputFormat.TYPE_GRAPHQL)).toBe('number')
			expect(typeMapper.mapScalarType('Boolean', OutputFormat.TYPE_GRAPHQL)).toBe('boolean')
			expect(typeMapper.mapScalarType('Float', OutputFormat.TYPE_GRAPHQL)).toBe('number')
		})

		it('should map extended scalar types correctly', () => {
			expect(typeMapper.mapScalarType('DateTime', OutputFormat.GRAPHQL)).toBe('Date')
			expect(typeMapper.mapScalarType('Json', OutputFormat.GRAPHQL)).toBe('GraphQLJSON')
			expect(typeMapper.mapScalarType('Decimal', OutputFormat.GRAPHQL)).toBe('Float')
			expect(typeMapper.mapScalarType('Bytes', OutputFormat.GRAPHQL)).toBe('String')
		})

		it('should handle unknown scalar types', () => {
			expect(typeMapper.mapScalarType('UnknownType', OutputFormat.GRAPHQL)).toBe('UnknownType')
			expect(typeMapper.mapScalarType('UnknownType', OutputFormat.TYPE_GRAPHQL)).toBe('unknowntype')
		})
	})

	describe('GraphQL Scalar Names', () => {
		it('should get correct GraphQL scalar names', () => {
			expect(typeMapper.getGraphQLScalarName('Json')).toBe('JSON')
			expect(typeMapper.getGraphQLScalarName('String')).toBeUndefined()
		})
	})

	describe('Type Utilities', () => {
		it('should identify scalar types correctly', () => {
			expect(typeMapper.isScalarType('String')).toBe(true)
			expect(typeMapper.isScalarType('Int')).toBe(true)
			expect(typeMapper.isScalarType('DateTime')).toBe(false)
			expect(typeMapper.isScalarType('User')).toBe(false)
		})

		it('should identify enum types correctly', () => {
			expect(typeMapper.isEnumType('Status')).toBe(true)
			expect(typeMapper.isEnumType('Priority')).toBe(true)
			expect(typeMapper.isEnumType('String')).toBe(false)
			expect(typeMapper.isEnumType('User')).toBe(false)
		})

		it('should identify model types correctly', () => {
			expect(typeMapper.isModelType('User')).toBe(true)
			expect(typeMapper.isModelType('Post')).toBe(true)
			expect(typeMapper.isModelType('String')).toBe(false)
			expect(typeMapper.isModelType('Status')).toBe(false)
		})

		it('should identify relation fields correctly', () => {
			const scalarField = createTestField('name', 'String')
			const relationField = createTestRelationField('user', 'User')

			expect(typeMapper.isRelationField(scalarField)).toBe(false)
			expect(typeMapper.isRelationField(relationField)).toBe(true)
		})
	})

	describe('Field Type Categories', () => {
		it('should categorize fields correctly', () => {
			const stringField = createTestField('name', 'String')
			const relationField = createTestRelationField('user', 'User')
			const enumField = {
				$type: 'DataModelField',
				name: 'status',
				type: {
					type: undefined as any,
					optional: false,
					array: false,
					reference: {
						ref: { name: 'Status' },
					},
				},
				attributes: [],
			} as any

			expect(typeMapper.getFieldTypeCategory(stringField)).toBe(FieldTypeCategory.SCALAR)
			expect(typeMapper.getFieldTypeCategory(relationField)).toBe(FieldTypeCategory.OBJECT)
			expect(typeMapper.getFieldTypeCategory(enumField)).toBe(FieldTypeCategory.ENUM)
		})
	})

	describe('Supported Types', () => {
		it('should return supported types list', () => {
			const supportedTypes = typeMapper.getSupportedTypes()
			expect(Array.isArray(supportedTypes)).toBe(true)
			expect(supportedTypes.length).toBeGreaterThan(0)
			expect(supportedTypes).toContain('String')
			expect(supportedTypes).toContain('Int')
		})

		it('should check if type has mapping', () => {
			expect(typeMapper.hasTypeMapping('String')).toBe(true)
			expect(typeMapper.hasTypeMapping('Int')).toBe(true)
			expect(typeMapper.hasTypeMapping('UnknownType')).toBe(false)
		})
	})

	describe('Custom Mappings', () => {
		it('should add custom type mapping', () => {
			typeMapper.addCustomMapping('CustomType', {
				graphql: 'CustomGraphQLType',
				typescript: 'CustomTSType',
			})

			expect(typeMapper.hasTypeMapping('CustomType')).toBe(true)
			expect(typeMapper.mapScalarType('CustomType', OutputFormat.GRAPHQL)).toBe('CustomGraphQLType')
			expect(typeMapper.mapScalarType('CustomType', OutputFormat.TYPE_GRAPHQL)).toBe('CustomTSType')
		})
	})

	describe('Field Decorators and Properties', () => {
		it('should get field decorator args', () => {
			const field = createTestField('testField', 'String', false, false)
			const args = typeMapper.getFieldDecoratorArgs(field, OutputFormat.GRAPHQL)
			expect(Array.isArray(args)).toBe(true)
		})

		it('should get property type string', () => {
			const field = createTestField('testField', 'String', false, false)
			const result = typeMapper.getPropertyTypeString(field, OutputFormat.GRAPHQL)
			expect(typeof result).toBe('string')
		})

		it('should get relation field type', () => {
			const relationField = createTestRelationField('user', 'User')
			const result = typeMapper.getRelationFieldType(relationField)
			expect(result).toBe('User!')
		})
	})

	describe('Error Handling', () => {
		it('should handle malformed fields gracefully', () => {
			const malformedField = {
				...createTestField('testField', 'String'),
				type: null as any,
			}

			const result = typeMapper.mapFieldType(malformedField)
			expect(result).toBeNull()
		})

		it('should handle fields with undefined type gracefully', () => {
			const undefinedTypeField = {
				...createTestField('testField', 'String'),
				type: {
					type: undefined,
					optional: false,
					array: false,
					reference: null,
				},
			} as any

			const result = typeMapper.mapFieldType(undefinedTypeField)
			expect(result).toBe('String!')
		})
	})

	describe('Complex Scenarios', () => {
		it('should handle fields with all modifier combinations', () => {
			const requiredScalar = createTestField('field1', 'String', false, false)
			const optionalScalar = createTestField('field2', 'String', true, false)
			const requiredArray = createTestField('field3', 'String', false, true)
			const optionalArray = createTestField('field4', 'String', true, true)

			expect(typeMapper.mapFieldType(requiredScalar)).toBe('String!')
			expect(typeMapper.mapFieldType(optionalScalar)).toBe('String')
			expect(typeMapper.mapFieldType(requiredArray)).toBe('[String!]!')
			expect(typeMapper.mapFieldType(optionalArray)).toBe('[String]')
		})

		it('should handle custom enum fields correctly', () => {
			const enumField = createTestField('status', 'Status', false, false)
			const result = typeMapper.mapFieldType(enumField)
			expect(result).toBe('Status!')
		})

		it('should handle different naming conventions', () => {
			const camelFormatter = new TypeFormatter('camelCase', 'snake_case')
			const camelMapper = new UnifiedTypeMapper(camelFormatter, models, enums, options)

			const field = createTestField('testField', 'String', false, false)
			const result = camelMapper.mapFieldType(field)
			expect(result).toBe('String!')
		})
	})

	describe('Performance with Large Data', () => {
		it('should handle large number of models efficiently', () => {
			const manyModels: DataModel[] = []
			for (let i = 0; i < 100; i++) {
				manyModels.push(TestFixtures.createDataModel(`Model${i}`, [TestFixtures.createField('id', 'String')]))
			}

			const performanceMapper = new UnifiedTypeMapper(typeFormatter, manyModels, enums, options)

			expect(performanceMapper.isModelType('Model0')).toBe(true)
			expect(performanceMapper.isModelType('Model99')).toBe(true)
			expect(performanceMapper.isModelType('Model100')).toBe(false)
		})

		it('should handle large number of enums efficiently', () => {
			const manyEnums: Enum[] = []
			for (let i = 0; i < 50; i++) {
				manyEnums.push(TestFixtures.createEnum(`Enum${i}`, [`VALUE${i}_A`]))
			}

			const performanceMapper = new UnifiedTypeMapper(typeFormatter, models, manyEnums, options)

			expect(performanceMapper.isEnumType('Enum0')).toBe(true)
			expect(performanceMapper.isEnumType('Enum49')).toBe(true)
			expect(performanceMapper.isEnumType('Enum50')).toBe(false)
		})
	})
})
