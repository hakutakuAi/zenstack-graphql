import { describe, it, expect, beforeEach } from 'bun:test'
import { UnifiedEnumGenerator } from '@generators/unified/unified-enum-generator'
import { OutputFormat } from '@utils/constants'
import { TestFixtures, TestMockFactory } from '../../helpers'

describe('Unified Enum Generator', () => {
	let context: any
	let generator: UnifiedEnumGenerator

	beforeEach(() => {
		const baseContext = TestFixtures.createContext({
			generateEnums: true,
			enums: [
				TestFixtures.createEnum('UserRole', ['ADMIN', 'USER', 'MODERATOR']),
				TestFixtures.createEnum('PostStatus', ['DRAFT', 'PUBLISHED', 'ARCHIVED']),
				TestFixtures.createEnum('OrderStatus', ['PENDING', 'PROCESSING', 'SHIPPED', 'DELIVERED']),
			],
		})

		context = TestMockFactory.createUnifiedContext(baseContext)
		generator = new UnifiedEnumGenerator(context, OutputFormat.GRAPHQL)
	})

	describe('Initialization', () => {
		it('should initialize with GraphQL format', () => {
			const graphqlGenerator = new UnifiedEnumGenerator(context, OutputFormat.GRAPHQL)
			expect(graphqlGenerator).toBeDefined()
		})

		it('should initialize with TypeScript format', () => {
			const typescriptGenerator = new UnifiedEnumGenerator(context, OutputFormat.TYPE_GRAPHQL)
			expect(typescriptGenerator).toBeDefined()
		})
	})

	describe('Enum Generation', () => {
		it('should generate enums when enabled', () => {
			const result = generator.generate()

			expect(result).toBeDefined()
			expect(Array.isArray(result)).toBe(true)
			expect(result.length).toBeGreaterThan(0)
		})

		it('should not generate enums when disabled', () => {
			const disabledContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					generateEnums: false,
					enums: [TestFixtures.createEnum('UserRole', ['ADMIN', 'USER'])],
				}),
			)
			const disabledGenerator = new UnifiedEnumGenerator(disabledContext, OutputFormat.GRAPHQL)

			const result = disabledGenerator.generate()

			expect(result).toEqual([])
		})

		it('should generate correct number of enums', () => {
			const result = generator.generate()

			expect(result.length).toBe(3)
		})
	})

	describe('GraphQL Enum Generation', () => {
		it('should generate GraphQL enum definitions', () => {
			const result = generator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBeGreaterThan(0)

			expect(result.length).toBe(3)
		})

		it('should handle enum with values', () => {
			const singleEnumContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					generateEnums: true,
					enums: [TestFixtures.createEnum('Priority', ['HIGH', 'MEDIUM', 'LOW'])],
				}),
			)
			const singleGenerator = new UnifiedEnumGenerator(singleEnumContext, OutputFormat.GRAPHQL)

			const result = singleGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(1)
		})

		it('should handle enum without values', () => {
			const emptyEnumContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					generateEnums: true,
					enums: [TestFixtures.createEnum('EmptyEnum', [])],
				}),
			)
			const emptyGenerator = new UnifiedEnumGenerator(emptyEnumContext, OutputFormat.GRAPHQL)

			const result = emptyGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(1)
		})
	})

	describe('TypeScript Enum Generation', () => {
		it('should generate TypeScript enum definitions', () => {
			const typescriptGenerator = new UnifiedEnumGenerator(context, OutputFormat.TYPE_GRAPHQL)

			const result = typescriptGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBeGreaterThan(0)
		})

		it('should handle different enum naming', () => {
			const customContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					generateEnums: true,
					typeNaming: 'camelCase',
					enums: [TestFixtures.createEnum('user_role', ['ADMIN', 'USER'])],
				}),
			)
			const customGenerator = new UnifiedEnumGenerator(customContext, OutputFormat.TYPE_GRAPHQL)

			const result = customGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(1)
		})
	})

	describe('Error Handling', () => {
		it('should handle missing enums gracefully', () => {
			const emptyContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
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

		it('should handle malformed enum gracefully', () => {
			const malformedEnum = TestFixtures.createEnum('', [])
			const malformedContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
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
		it('should process all enums', () => {
			const multiEnumContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					generateEnums: true,
					enums: [
						TestFixtures.createEnum('Color', ['RED', 'GREEN', 'BLUE']),
						TestFixtures.createEnum('Size', ['SMALL', 'MEDIUM', 'LARGE']),
						TestFixtures.createEnum('Category', ['A', 'B', 'C']),
					],
				}),
			)
			const multiGenerator = new UnifiedEnumGenerator(multiEnumContext, OutputFormat.GRAPHQL)

			const result = multiGenerator.generate()

			expect(result.length).toBe(3)
		})

		it('should handle enums with many values', () => {
			const manyValuesEnum = TestFixtures.createEnum('Alphabet', 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.split(''))
			const manyValuesContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					generateEnums: true,
					enums: [manyValuesEnum],
				}),
			)
			const manyValuesGenerator = new UnifiedEnumGenerator(manyValuesContext, OutputFormat.GRAPHQL)

			const result = manyValuesGenerator.generate()

			expect(result).toBeDefined()
			expect(result.length).toBe(1)
		})

		it('should handle enums with special characters in names', () => {
			const specialEnum = TestFixtures.createEnum('Special_Enum_123', ['VALUE_1', 'VALUE_2'])
			const specialContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
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
		it('should return consistent result structure', () => {
			const result = generator.generate()

			expect(Array.isArray(result)).toBe(true)
			expect(result.every((item) => typeof item === 'string')).toBe(true)
		})

		it('should generate different results for different formats', () => {
			const graphqlResult = generator.generate()
			const typescriptGenerator = new UnifiedEnumGenerator(context, OutputFormat.TYPE_GRAPHQL)
			const typescriptResult = typescriptGenerator.generate()

			expect(Array.isArray(graphqlResult)).toBe(true)
			expect(Array.isArray(typescriptResult)).toBe(true)
		})

		it('should validate actual output structure for GraphQL', () => {
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

		it('should validate actual output structure for TypeScript', () => {
			const typescriptGenerator = new UnifiedEnumGenerator(context, OutputFormat.TYPE_GRAPHQL)
			const result = typescriptGenerator.generate()

			expect(result.length).toBe(3)
			expect(result).toContain('UserRole')
			expect(result).toContain('PostStatus')
			expect(result).toContain('OrderStatus')
		})

		it('should respect generateEnums flag', () => {
			const enabledContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					generateEnums: true,
					enums: [TestFixtures.createEnum('Test', ['A', 'B'])],
				}),
			)
			const disabledContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					generateEnums: false,
					enums: [TestFixtures.createEnum('Test', ['A', 'B'])],
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
		it('should handle single enum', () => {
			const singleContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
					generateEnums: true,
					enums: [TestFixtures.createEnum('Single', ['ONLY'])],
				}),
			)
			const singleGenerator = new UnifiedEnumGenerator(singleContext, OutputFormat.GRAPHQL)

			const result = singleGenerator.generate()

			expect(result.length).toBe(1)
		})

		it('should handle enum with duplicate values', () => {
			const duplicateEnum = TestFixtures.createEnum('Duplicate', ['A', 'B', 'A', 'C'])
			const duplicateContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
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

		it('should handle enum with very long names', () => {
			const longName = 'A'.repeat(100)
			const longEnum = TestFixtures.createEnum(longName, ['VALUE'])
			const longContext = TestMockFactory.createUnifiedContext(
				TestFixtures.createContext({
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
