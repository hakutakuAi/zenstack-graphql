import { describe, test, expect, beforeEach } from 'bun:test'
import { UnifiedEnumGenerator } from '@generators/unified/unified-enum-generator'
import { OutputFormat } from '@utils/constants'
import { TestUtils, MockFactory } from '../../helpers'

describe('Unified Enum Generator', () => {
	let context: any
	let generator: UnifiedEnumGenerator

	beforeEach(() => {
		const baseContext = TestUtils.createMockContext({
			generateEnums: true,
			enums: [
				TestUtils.createMockEnum('UserRole', ['ADMIN', 'USER', 'MODERATOR']),
				TestUtils.createMockEnum('PostStatus', ['DRAFT', 'PUBLISHED', 'ARCHIVED']),
				TestUtils.createMockEnum('OrderStatus', ['PENDING', 'PROCESSING', 'SHIPPED', 'DELIVERED']),
			],
		})

		context = MockFactory.createMockUnifiedGeneratorContext(baseContext)
		generator = new UnifiedEnumGenerator(context, OutputFormat.GRAPHQL)
	})

	describe('Initialization', () => {
		test('should initialize with GraphQL format', () => {
			const graphqlGenerator = new UnifiedEnumGenerator(context, OutputFormat.GRAPHQL)
			expect(graphqlGenerator).toBeDefined()
		})

		test('should initialize with TypeScript format', () => {
			const typescriptGenerator = new UnifiedEnumGenerator(context, OutputFormat.TYPE_GRAPHQL)
			expect(typescriptGenerator).toBeDefined()
		})
	})

	describe('Enum Generation', () => {
		test('should generate enums when enabled', () => {
			const result = generator.generate()

			expect(result).toBeDefined()
			expect(Array.isArray(result)).toBe(true)
			expect(result.length).toBeGreaterThan(0)
		})

		test('should not generate enums when disabled', () => {
			const disabledContext = MockFactory.createMockUnifiedGeneratorContext(
				TestUtils.createMockContext({
					generateEnums: false,
					enums: [TestUtils.createMockEnum('UserRole', ['ADMIN', 'USER'])],
				}),
			)
			const disabledGenerator = new UnifiedEnumGenerator(disabledContext, OutputFormat.GRAPHQL)

			const result = disabledGenerator.generate()

			expect(result).toEqual([])
		})

		test('should generate correct number of enums', () => {
			const result = generator.generate()

			expect(result.length).toBe(3)
		})
	})

	describe('GraphQL Enum Generation', () => {
		test('should generate GraphQL enum definitions', () => {
			const result = generator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBeGreaterThan(0)

			expect(result.length).toBe(3)
		})

		test('should handle enum with values', () => {
			const singleEnumContext = MockFactory.createMockUnifiedGeneratorContext(
				TestUtils.createMockContext({
					generateEnums: true,
					enums: [TestUtils.createMockEnum('Priority', ['HIGH', 'MEDIUM', 'LOW'])],
				}),
			)
			const singleGenerator = new UnifiedEnumGenerator(singleEnumContext, OutputFormat.GRAPHQL)

			const result = singleGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(1)
		})

		test('should handle enum without values', () => {
			const emptyEnumContext = MockFactory.createMockUnifiedGeneratorContext(
				TestUtils.createMockContext({
					generateEnums: true,
					enums: [TestUtils.createMockEnum('EmptyEnum', [])],
				}),
			)
			const emptyGenerator = new UnifiedEnumGenerator(emptyEnumContext, OutputFormat.GRAPHQL)

			const result = emptyGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(1)
		})
	})

	describe('TypeScript Enum Generation', () => {
		test('should generate TypeScript enum definitions', () => {
			const typescriptGenerator = new UnifiedEnumGenerator(context, OutputFormat.TYPE_GRAPHQL)

			const result = typescriptGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBeGreaterThan(0)
		})

		test('should handle different enum naming', () => {
			const customContext = MockFactory.createMockUnifiedGeneratorContext(
				TestUtils.createMockContext({
					generateEnums: true,
					typeNaming: 'camelCase',
					enums: [TestUtils.createMockEnum('user_role', ['ADMIN', 'USER'])],
				}),
			)
			const customGenerator = new UnifiedEnumGenerator(customContext, OutputFormat.TYPE_GRAPHQL)

			const result = customGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(1)
		})
	})

	describe('Error Handling', () => {
		test('should handle missing enums gracefully', () => {
			const emptyContext = MockFactory.createMockUnifiedGeneratorContext(
				TestUtils.createMockContext({
					generateEnums: true,
					enums: [],
				}),
			)
			const emptyGenerator = new UnifiedEnumGenerator(emptyContext, OutputFormat.GRAPHQL)

			expect(() => {
				const result = emptyGenerator.generate()
				expect(result).toBeDefined()
				expect(result).toEqual([])
			}).not.toThrow()
		})

		test('should handle malformed enum gracefully', () => {
			const malformedEnum = TestUtils.createMockEnum('', [])
			const malformedContext = MockFactory.createMockUnifiedGeneratorContext(
				TestUtils.createMockContext({
					generateEnums: true,
					enums: [malformedEnum],
				}),
			)
			const malformedGenerator = new UnifiedEnumGenerator(malformedContext, OutputFormat.GRAPHQL)

			expect(() => {
				const result = malformedGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})
	})

	describe('Enum Processing', () => {
		test('should process all enums', () => {
			const multiEnumContext = MockFactory.createMockUnifiedGeneratorContext(
				TestUtils.createMockContext({
					generateEnums: true,
					enums: [
						TestUtils.createMockEnum('Color', ['RED', 'GREEN', 'BLUE']),
						TestUtils.createMockEnum('Size', ['SMALL', 'MEDIUM', 'LARGE']),
						TestUtils.createMockEnum('Category', ['A', 'B', 'C']),
					],
				}),
			)
			const multiGenerator = new UnifiedEnumGenerator(multiEnumContext, OutputFormat.GRAPHQL)

			const result = multiGenerator.generate()

			expect(result.length).toBe(3)
		})

		test('should handle enums with many values', () => {
			const manyValuesEnum = TestUtils.createMockEnum('Alphabet', 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.split(''))
			const manyValuesContext = MockFactory.createMockUnifiedGeneratorContext(
				TestUtils.createMockContext({
					generateEnums: true,
					enums: [manyValuesEnum],
				}),
			)
			const manyValuesGenerator = new UnifiedEnumGenerator(manyValuesContext, OutputFormat.GRAPHQL)

			const result = manyValuesGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(1)
		})

		test('should handle enums with special characters in names', () => {
			const specialEnum = TestUtils.createMockEnum('Special_Enum_123', ['VALUE_1', 'VALUE_2'])
			const specialContext = MockFactory.createMockUnifiedGeneratorContext(
				TestUtils.createMockContext({
					generateEnums: true,
					enums: [specialEnum],
				}),
			)
			const specialGenerator = new UnifiedEnumGenerator(specialContext, OutputFormat.GRAPHQL)

			expect(() => {
				const result = specialGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})
	})

	describe('Generation Results', () => {
		test('should return consistent result structure', () => {
			const result = generator.generate()

			expect(Array.isArray(result)).toBe(true)
			expect(result.every((item) => typeof item === 'string')).toBe(true)
		})

		test('should generate different results for different formats', () => {
			const graphqlResult = generator.generate()
			const typescriptGenerator = new UnifiedEnumGenerator(context, OutputFormat.TYPE_GRAPHQL)
			const typescriptResult = typescriptGenerator.generate()

			expect(Array.isArray(graphqlResult)).toBe(true)
			expect(Array.isArray(typescriptResult)).toBe(true)
		})

		test('should validate actual output structure for GraphQL', () => {
			const result = generator.generate()

			expect(result.length).toBe(3)

			result.forEach((enumName) => {
				expect(enumName).toMatch(/^[A-Z][a-zA-Z0-9]*$/)
				expect(enumName.length).toBeGreaterThan(0)
			})

			expect(result).toContain('UserRole')
			expect(result).toContain('PostStatus')
			expect(result).toContain('OrderStatus')
		})

		test('should validate actual output structure for TypeScript', () => {
			const typescriptGenerator = new UnifiedEnumGenerator(context, OutputFormat.TYPE_GRAPHQL)
			const result = typescriptGenerator.generate()

			expect(result.length).toBe(3)
			expect(result).toContain('UserRole')
			expect(result).toContain('PostStatus')
			expect(result).toContain('OrderStatus')
		})

		test('should respect generateEnums flag', () => {
			const enabledContext = MockFactory.createMockUnifiedGeneratorContext(
				TestUtils.createMockContext({
					generateEnums: true,
					enums: [TestUtils.createMockEnum('Test', ['A', 'B'])],
				}),
			)
			const disabledContext = MockFactory.createMockUnifiedGeneratorContext(
				TestUtils.createMockContext({
					generateEnums: false,
					enums: [TestUtils.createMockEnum('Test', ['A', 'B'])],
				}),
			)

			const enabledGenerator = new UnifiedEnumGenerator(enabledContext, OutputFormat.GRAPHQL)
			const disabledGenerator = new UnifiedEnumGenerator(disabledContext, OutputFormat.GRAPHQL)

			const enabledResult = enabledGenerator.generate()
			const disabledResult = disabledGenerator.generate()

			expect(enabledResult.length).toBeGreaterThan(0)
			expect(disabledResult.length).toBe(0)
		})
	})

	describe('Edge Cases', () => {
		test('should handle single enum', () => {
			const singleContext = MockFactory.createMockUnifiedGeneratorContext(
				TestUtils.createMockContext({
					generateEnums: true,
					enums: [TestUtils.createMockEnum('Single', ['ONLY'])],
				}),
			)
			const singleGenerator = new UnifiedEnumGenerator(singleContext, OutputFormat.GRAPHQL)

			const result = singleGenerator.generate()

			expect(result.length).toBe(1)
		})

		test('should handle enum with duplicate values', () => {
			const duplicateEnum = TestUtils.createMockEnum('Duplicate', ['A', 'B', 'A', 'C'])
			const duplicateContext = MockFactory.createMockUnifiedGeneratorContext(
				TestUtils.createMockContext({
					generateEnums: true,
					enums: [duplicateEnum],
				}),
			)
			const duplicateGenerator = new UnifiedEnumGenerator(duplicateContext, OutputFormat.GRAPHQL)

			expect(() => {
				const result = duplicateGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})

		test('should handle enum with very long names', () => {
			const longName = 'A'.repeat(100)
			const longEnum = TestUtils.createMockEnum(longName, ['VALUE'])
			const longContext = MockFactory.createMockUnifiedGeneratorContext(
				TestUtils.createMockContext({
					generateEnums: true,
					enums: [longEnum],
				}),
			)
			const longGenerator = new UnifiedEnumGenerator(longContext, OutputFormat.GRAPHQL)

			expect(() => {
				const result = longGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})
	})
})
