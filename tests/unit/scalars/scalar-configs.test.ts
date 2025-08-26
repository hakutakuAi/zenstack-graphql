import { describe, it, expect } from 'bun:test'
import {
	createDateTimeScalarConfig,
	createJSONScalarConfig,
	createDecimalScalarConfig,
	createCustomScalarConfig,
	createGraphQLScalarType,
	isBuiltinGraphQLScalar,
	isBuiltinPrismaType,
	isExtendedBuiltinScalar,
	getScalarDefinition,
	UNIFIED_SCALAR_DEFINITIONS,
	BUILTIN_GRAPHQL_SCALARS,
	BUILTIN_PRISMA_TYPES,
	EXTENDED_BUILTIN_SCALARS,
} from '@utils/constants'
import { Kind } from 'graphql/language'
import { GraphQLError } from 'graphql'

describe('Scalar Configuration Functions', () => {
	describe('DateTime Scalar Configuration', () => {
		it('should create correct DateTime scalar configuration', () => {
			const config = createDateTimeScalarConfig()

			expect(config.name).toBe('DateTime')
			expect(config.description).toBe('A date-time string at UTC, such as 2007-12-03T10:15:30Z')
			expect(typeof config.serialize).toBe('function')
			expect(typeof config.parseValue).toBe('function')
			expect(typeof config.parseLiteral).toBe('function')
		})

		it('should serialize Date objects correctly', () => {
			const config = createDateTimeScalarConfig()
			const date = new Date('2023-12-03T10:15:30Z')

			const serialized = config.serialize(date)
			expect(serialized).toBe('2023-12-03T10:15:30.000Z')
		})

		it('should serialize valid date strings correctly', () => {
			const config = createDateTimeScalarConfig()
			const dateString = '2023-12-03T10:15:30Z'

			const serialized = config.serialize(dateString)
			expect(serialized).toBe('2023-12-03T10:15:30.000Z')
		})

		it('should throw error for invalid date serialization', () => {
			const config = createDateTimeScalarConfig()

			expect(() => config.serialize('invalid-date')).toThrow(GraphQLError)
			expect(() => config.serialize(123)).toThrow(GraphQLError)
			expect(() => config.serialize(null)).toThrow(GraphQLError)
		})

		it('should throw error for invalid Date object', () => {
			const config = createDateTimeScalarConfig()
			const invalidDate = new Date('invalid')

			expect(() => config.serialize(invalidDate)).toThrow(GraphQLError)
		})

		it('should parse valid date strings correctly', () => {
			const config = createDateTimeScalarConfig()
			const dateString = '2023-12-03T10:15:30Z'

			const parsed = config.parseValue(dateString)
			expect(parsed).toBeInstanceOf(Date)
			expect(parsed.toISOString()).toBe('2023-12-03T10:15:30.000Z')
		})

		it('should throw error for invalid parseValue input', () => {
			const config = createDateTimeScalarConfig()

			expect(() => config.parseValue(123)).toThrow(GraphQLError)
			expect(() => config.parseValue('invalid-date')).toThrow(GraphQLError)
		})

		it('should parse string literals correctly', () => {
			const config = createDateTimeScalarConfig()
			const ast = { kind: Kind.STRING, value: '2023-12-03T10:15:30Z' }

			const parsed = config.parseLiteral(ast)
			expect(parsed).toBeInstanceOf(Date)
			expect(parsed.toISOString()).toBe('2023-12-03T10:15:30.000Z')
		})

		it('should throw error for non-string literals', () => {
			const config = createDateTimeScalarConfig()
			const ast = { kind: Kind.INT, value: '123' }

			expect(() => config.parseLiteral(ast)).toThrow(GraphQLError)
		})

		it('should throw error for invalid string literals', () => {
			const config = createDateTimeScalarConfig()
			const ast = { kind: Kind.STRING, value: 'invalid-date' }

			expect(() => config.parseLiteral(ast)).toThrow(GraphQLError)
		})
	})

	describe('JSON Scalar Configuration', () => {
		it('should create correct JSON scalar configuration', () => {
			const config = createJSONScalarConfig()

			expect(config.name).toBe('JSON')
			expect(config.description).toBe('The `JSON` scalar type represents JSON values as specified by ECMA-404')
			expect(typeof config.serialize).toBe('function')
			expect(typeof config.parseValue).toBe('function')
			expect(typeof config.parseLiteral).toBe('function')
		})

		it('should serialize objects correctly', () => {
			const config = createJSONScalarConfig()
			const obj = { name: 'John', age: 30, active: true }

			const serialized = config.serialize(obj)
			expect(serialized).toEqual(obj)
		})

		it('should serialize arrays correctly', () => {
			const config = createJSONScalarConfig()
			const arr = [1, 2, 3, 'test', { nested: true }]

			const serialized = config.serialize(arr)
			expect(serialized).toEqual(arr)
		})

		it('should serialize strings as JSON if possible', () => {
			const config = createJSONScalarConfig()
			const jsonString = '{"name": "John", "age": 30}'

			const serialized = config.serialize(jsonString)
			expect(serialized).toEqual({ name: 'John', age: 30 })
		})

		it('should serialize invalid JSON strings as-is', () => {
			const config = createJSONScalarConfig()
			const invalidJson = 'not json'

			const serialized = config.serialize(invalidJson)
			expect(serialized).toBe('not json')
		})

		it('should serialize null and undefined correctly', () => {
			const config = createJSONScalarConfig()

			expect(config.serialize(null)).toBeNull()
			expect(config.serialize(undefined)).toBeNull()
		})

		it('should parse valid JSON strings', () => {
			const config = createJSONScalarConfig()
			const jsonString = '{"name": "John", "age": 30}'

			const parsed = config.parseValue(jsonString)
			expect(parsed).toEqual({ name: 'John', age: 30 })
		})

		it('should throw error for invalid JSON strings in parseValue', () => {
			const config = createJSONScalarConfig()
			const invalidJson = '{invalid json}'

			expect(() => config.parseValue(invalidJson)).toThrow(GraphQLError)
		})

		it('should return non-string values as-is in parseValue', () => {
			const config = createJSONScalarConfig()
			const obj = { name: 'John' }

			const parsed = config.parseValue(obj)
			expect(parsed).toEqual(obj)
		})

		describe('parseLiteral', () => {
			it('should parse STRING literals as JSON', () => {
				const config = createJSONScalarConfig()
				const ast = { kind: Kind.STRING, value: '{"name": "John"}' }

				const parsed = config.parseLiteral(ast)
				expect(parsed).toEqual({ name: 'John' })
			})

			it('should parse invalid JSON STRING literals as string', () => {
				const config = createJSONScalarConfig()
				const ast = { kind: Kind.STRING, value: 'not json' }

				const parsed = config.parseLiteral(ast)
				expect(parsed).toBe('not json')
			})

			it('should parse INT literals', () => {
				const config = createJSONScalarConfig()
				const ast = { kind: Kind.INT, value: '42' }

				const parsed = config.parseLiteral(ast)
				expect(parsed).toBe(42)
			})

			it('should parse FLOAT literals', () => {
				const config = createJSONScalarConfig()
				const ast = { kind: Kind.FLOAT, value: '3.14' }

				const parsed = config.parseLiteral(ast)
				expect(parsed).toBe(3.14)
			})

			it('should parse BOOLEAN literals', () => {
				const config = createJSONScalarConfig()
				const astTrue = { kind: Kind.BOOLEAN, value: 'true' }
				const astFalse = { kind: Kind.BOOLEAN, value: 'false' }

				expect(config.parseLiteral(astTrue)).toBe('true')
				expect(config.parseLiteral(astFalse)).toBe('false')
			})

			it('should parse NULL literals', () => {
				const config = createJSONScalarConfig()
				const ast = { kind: Kind.NULL, value: 'null' }

				const parsed = config.parseLiteral(ast)
				expect(parsed).toBeNull()
			})

			it('should parse OBJECT literals', () => {
				const config = createJSONScalarConfig()
				const ast = {
					kind: Kind.OBJECT,
					fields: [
						{
							name: { value: 'name' },
							value: { kind: Kind.STRING, value: 'John' },
						},
						{
							name: { value: 'age' },
							value: { kind: Kind.INT, value: '30' },
						},
					],
				}

				const parsed = config.parseLiteral(ast)
				expect(parsed).toEqual({ name: 'John', age: 30 })
			})

			it('should parse LIST literals', () => {
				const config = createJSONScalarConfig()
				const ast = {
					kind: Kind.LIST,
					values: [
						{ kind: Kind.STRING, value: 'item1' },
						{ kind: Kind.INT, value: '42' },
					],
				}

				const parsed = config.parseLiteral(ast)
				expect(parsed).toEqual(['item1', 42])
			})

			it('should throw error for unsupported literal kinds', () => {
				const config = createJSONScalarConfig()
				const ast = { kind: 'UNKNOWN_KIND', value: 'test' }

				expect(() => config.parseLiteral(ast)).toThrow(GraphQLError)
			})
		})
	})

	describe('Decimal Scalar Configuration', () => {
		it('should create correct Decimal scalar configuration', () => {
			const config = createDecimalScalarConfig()

			expect(config.name).toBe('Decimal')
			expect(config.description).toBe('An arbitrary-precision Decimal type')
			expect(typeof config.serialize).toBe('function')
			expect(typeof config.parseValue).toBe('function')
			expect(typeof config.parseLiteral).toBe('function')
		})

		it('should serialize numbers correctly', () => {
			const config = createDecimalScalarConfig()

			expect(config.serialize(42)).toBe('42')
			expect(config.serialize(3.14159)).toBe('3.14159')
			expect(config.serialize(0)).toBe('0')
		})

		it('should serialize string numbers correctly', () => {
			const config = createDecimalScalarConfig()

			expect(config.serialize('42')).toBe('42')
			expect(config.serialize('3.14159')).toBe('3.14159')
			expect(config.serialize('-123.456')).toBe('-123.456')
		})

		it('should serialize objects with toString method', () => {
			const config = createDecimalScalarConfig()
			const decimalLike = {
				toString: () => '123.456',
			}

			const serialized = config.serialize(decimalLike)
			expect(serialized).toBe('123.456')
		})

		it('should handle null and undefined serialization', () => {
			const config = createDecimalScalarConfig()

			expect(config.serialize(null)).toBeNull()
			expect(config.serialize(undefined)).toBeNull()
		})

		it('should throw error for invalid number strings', () => {
			const config = createDecimalScalarConfig()

			expect(() => config.serialize('not a number')).toThrow(GraphQLError)
			expect(() => config.serialize('abc')).toThrow(GraphQLError)
		})

		it('should handle object with toString method', () => {
			const config = createDecimalScalarConfig()

			const result1 = config.serialize({})
			const result2 = config.serialize([])

			expect(result1).toBe('[object Object]')
			expect(result2).toBe('')
		})

		it('should parse number values correctly', () => {
			const config = createDecimalScalarConfig()

			expect(config.parseValue(42)).toBe('42')
			expect(config.parseValue(3.14)).toBe('3.14')
		})

		it('should parse string values correctly', () => {
			const config = createDecimalScalarConfig()

			expect(config.parseValue('42')).toBe('42')
			expect(config.parseValue('3.14159')).toBe('3.14159')
		})

		it('should throw error for invalid parseValue inputs', () => {
			const config = createDecimalScalarConfig()

			expect(() => config.parseValue('not a number')).toThrow(GraphQLError)
		})

		it('should parse literal values correctly', () => {
			const config = createDecimalScalarConfig()

			const stringAst = { kind: Kind.STRING, value: '42.5' }
			const intAst = { kind: Kind.INT, value: '42' }
			const floatAst = { kind: Kind.FLOAT, value: '3.14' }

			expect(config.parseLiteral(stringAst)).toBe('42.5')
			expect(config.parseLiteral(intAst)).toBe('42')
			expect(config.parseLiteral(floatAst)).toBe('3.14')
		})

		it('should throw error for invalid literal values', () => {
			const config = createDecimalScalarConfig()
			const invalidStringAst = { kind: Kind.STRING, value: 'not a number' }
			const booleanAst = { kind: Kind.BOOLEAN, value: 'true' }

			expect(() => config.parseLiteral(invalidStringAst)).toThrow(GraphQLError)
			expect(() => config.parseLiteral(booleanAst)).toThrow(GraphQLError)
		})
	})

	describe('Custom Scalar Configuration', () => {
		it('should create custom scalar configuration', () => {
			const config = createCustomScalarConfig('MyScalar', 'MyPrismaType')

			expect(config.name).toBe('MyScalar')
			expect(config.description).toBe('Custom scalar type for MyPrismaType')
			expect(typeof config.serialize).toBe('function')
			expect(typeof config.parseValue).toBe('function')
			expect(typeof config.parseLiteral).toBe('function')
		})

		it('should serialize values as strings', () => {
			const config = createCustomScalarConfig('MyScalar', 'MyType')

			expect(config.serialize('test')).toBe('test')
			expect(config.serialize(123)).toBe('123')
			expect(config.serialize(true)).toBe('true')
			expect(config.serialize(null)).toBeNull()
			expect(config.serialize(undefined)).toBeNull()
		})

		it('should parse values as strings', () => {
			const config = createCustomScalarConfig('MyScalar', 'MyType')

			expect(config.parseValue('test')).toBe('test')
			expect(config.parseValue(123)).toBe('123')
			expect(config.parseValue(true)).toBe('true')
		})

		it('should parse string literals only', () => {
			const config = createCustomScalarConfig('MyScalar', 'MyType')
			const stringAst = { kind: Kind.STRING, value: 'test' }

			expect(config.parseLiteral(stringAst)).toBe('test')
		})

		it('should throw error for non-string literals', () => {
			const config = createCustomScalarConfig('MyScalar', 'MyType')
			const intAst = { kind: Kind.INT, value: '123' }

			expect(() => config.parseLiteral(intAst)).toThrow(GraphQLError)
		})
	})

	describe('GraphQL Scalar Type Creation', () => {
		it('should create GraphQL scalar type from config', () => {
			const config = createDateTimeScalarConfig()
			const scalarType = createGraphQLScalarType(config)

			expect(scalarType).toBeDefined()
			expect(scalarType.name).toBe('DateTime')
			expect(scalarType.description).toBe('A date-time string at UTC, such as 2007-12-03T10:15:30Z')
		})

		it('should create functional GraphQL scalar type', () => {
			const config = createJSONScalarConfig()
			const scalarType = createGraphQLScalarType(config)

			const testValue = { name: 'John', age: 30 }
			const serialized = scalarType.serialize(testValue)
			expect(serialized).toEqual(testValue)
		})
	})

	describe('Type Checking Utilities', () => {
		describe('isBuiltinGraphQLScalar', () => {
			it('should identify builtin GraphQL scalars correctly', () => {
				expect(isBuiltinGraphQLScalar('String')).toBe(true)
				expect(isBuiltinGraphQLScalar('Int')).toBe(true)
				expect(isBuiltinGraphQLScalar('Float')).toBe(true)
				expect(isBuiltinGraphQLScalar('Boolean')).toBe(true)
				expect(isBuiltinGraphQLScalar('ID')).toBe(true)

				expect(isBuiltinGraphQLScalar('DateTime')).toBe(false)
				expect(isBuiltinGraphQLScalar('JSON')).toBe(false)
				expect(isBuiltinGraphQLScalar('CustomType')).toBe(false)
			})
		})

		describe('isBuiltinPrismaType', () => {
			it('should identify builtin Prisma types correctly', () => {
				expect(isBuiltinPrismaType('DateTime')).toBe(true)
				expect(isBuiltinPrismaType('Json')).toBe(true)
				expect(isBuiltinPrismaType('Decimal')).toBe(true)
				expect(isBuiltinPrismaType('Bytes')).toBe(true)

				expect(isBuiltinPrismaType('String')).toBe(false)
				expect(isBuiltinPrismaType('Int')).toBe(false)
				expect(isBuiltinPrismaType('CustomType')).toBe(false)
			})
		})

		describe('isExtendedBuiltinScalar', () => {
			it('should identify extended builtin scalars correctly', () => {
				expect(isExtendedBuiltinScalar('String')).toBe(true)
				expect(isExtendedBuiltinScalar('Int')).toBe(true)
				expect(isExtendedBuiltinScalar('Float')).toBe(true)
				expect(isExtendedBuiltinScalar('Boolean')).toBe(true)
				expect(isExtendedBuiltinScalar('ID')).toBe(true)
				expect(isExtendedBuiltinScalar('DateTime')).toBe(true)
				expect(isExtendedBuiltinScalar('JSON')).toBe(true)
				expect(isExtendedBuiltinScalar('Decimal')).toBe(true)

				expect(isExtendedBuiltinScalar('CustomType')).toBe(false)
			})
		})
	})

	describe('Scalar Definition Utilities', () => {
		it('should get scalar definition for known types', () => {
			const dateTimeDef = getScalarDefinition('DateTime')
			expect(dateTimeDef).toBeDefined()
			expect(dateTimeDef?.prismaType).toBe('DateTime')
			expect(dateTimeDef?.graphqlType).toBe('DateTime')
			expect(dateTimeDef?.typescriptType).toBe('Date')

			const jsonDef = getScalarDefinition('Json')
			expect(jsonDef).toBeDefined()
			expect(jsonDef?.prismaType).toBe('Json')
			expect(jsonDef?.graphqlType).toBe('JSON')
			expect(jsonDef?.typescriptType).toBe('any')

			const decimalDef = getScalarDefinition('Decimal')
			expect(decimalDef).toBeDefined()
			expect(decimalDef?.prismaType).toBe('Decimal')
			expect(decimalDef?.graphqlType).toBe('Decimal')
			expect(decimalDef?.typescriptType).toBe('number')
		})

		it('should return undefined for unknown types', () => {
			const unknownDef = getScalarDefinition('UnknownType')
			expect(unknownDef).toBeUndefined()
		})

		it('should have valid unified scalar definitions', () => {
			expect(UNIFIED_SCALAR_DEFINITIONS).toBeDefined()
			expect(Array.isArray(UNIFIED_SCALAR_DEFINITIONS)).toBe(true)
			expect(UNIFIED_SCALAR_DEFINITIONS.length).toBe(3)

			for (const definition of UNIFIED_SCALAR_DEFINITIONS) {
				expect(definition.prismaType).toBeDefined()
				expect(definition.graphqlType).toBeDefined()
				expect(definition.typescriptType).toBeDefined()
				expect(definition.description).toBeDefined()
				expect(typeof definition.createGraphQLConfig).toBe('function')
				expect(typeof definition.createTypeScriptType).toBe('function')
			}
		})

		it('should execute scalar definition functions correctly', () => {
			for (const definition of UNIFIED_SCALAR_DEFINITIONS) {
				const config = definition.createGraphQLConfig()
				expect(config).toBeDefined()
				expect(config.name).toBe(definition.graphqlType)

				const tsType = definition.createTypeScriptType()
				expect(tsType).toBe(definition.typescriptType)
			}
		})
	})

	describe('Constant Values', () => {
		it('should have correct builtin GraphQL scalars', () => {
			expect(BUILTIN_GRAPHQL_SCALARS).toEqual(['String', 'Int', 'Float', 'Boolean', 'ID'])
		})

		it('should have correct builtin Prisma types', () => {
			expect(BUILTIN_PRISMA_TYPES).toEqual(['DateTime', 'Json', 'Decimal', 'Bytes'])
		})

		it('should have correct extended builtin scalars', () => {
			expect(EXTENDED_BUILTIN_SCALARS).toEqual(['String', 'Int', 'Float', 'Boolean', 'ID', 'DateTime', 'JSON', 'Decimal'])
		})
	})
})
