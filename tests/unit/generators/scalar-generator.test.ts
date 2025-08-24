import { describe, test, expect, beforeEach } from 'bun:test'
import { UnifiedScalarGenerator } from '@generators/unified/unified-scalar-generator'
import { OutputFormat } from '@utils/constants'
import { TestUtils, MockFactory } from '../../helpers'
import { BaseGeneratorContext } from '@core/types'

describe('Unified Scalar Generator', () => {
	let context: any
	let generator: UnifiedScalarGenerator

	beforeEach(() => {
		context = MockFactory.createMockGraphQLContext(
			TestUtils.createMockContext({
				generateScalars: true,
				scalarTypes: {
					DateTime: 'DateTime',
					Json: 'JSON',
					Decimal: 'Decimal',
					Bytes: 'String',
				},
			}),
		)

		generator = new UnifiedScalarGenerator(context, OutputFormat.GRAPHQL)
	})

	describe('Initialization', () => {
		test('should initialize with GraphQL format', () => {
			const graphqlGenerator = new UnifiedScalarGenerator(context, OutputFormat.GRAPHQL)
			expect(graphqlGenerator).toBeDefined()
		})

		test('should initialize with TypeScript format', () => {
			const typescriptGenerator = new UnifiedScalarGenerator(context, OutputFormat.TYPE_GRAPHQL)
			expect(typescriptGenerator).toBeDefined()
		})
	})

	describe('Scalar Generation', () => {
		test('should generate default scalars when enabled', () => {
			const result = generator.generate()

			expect(result).toBeDefined()
			expect(result.graphqlTypes).toBeDefined()
			expect(result.graphqlTypes!.length).toBeGreaterThan(0)
		})

		test('should not generate scalars when disabled', () => {
			const disabledContext = TestUtils.createMockContext({
				generateScalars: false,
			})
			const disabledGenerator = new UnifiedScalarGenerator(disabledContext, OutputFormat.GRAPHQL)

			const result = disabledGenerator.generate()

			expect(result.graphqlTypes).toEqual([])
		})

		test('should use custom scalar mappings', () => {
			const customContext = TestUtils.createMockContext({
				generateScalars: true,
				scalarTypes: {
					DateTime: 'CustomDateTime',
					Json: 'CustomJSON',
				},
			})
			const customGenerator = new UnifiedScalarGenerator(customContext, OutputFormat.GRAPHQL)

			const result = customGenerator.generate()

			expect(result).toBeDefined()
			expect(result.graphqlTypes).toBeDefined()
		})
	})

	describe('GraphQL Scalar Generation', () => {
		test('should generate GraphQL scalar definitions', () => {
			const result = generator.generate()

			expect(result.graphqlTypes).toBeDefined()
			expect(result.graphqlTypes!.length).toBeGreaterThan(0)
		})

		test('should include standard scalars', () => {
			const result = generator.generate()

			expect(result.graphqlTypes).toBeDefined()
			expect(result.graphqlTypes!.length).toBeGreaterThanOrEqual(3)
		})

		test('should respect scalar type configuration', () => {
			const customScalarContext = MockFactory.createMockGraphQLContext(
				TestUtils.createMockContext({
					generateScalars: true,
					scalarTypes: {
						DateTime: 'Timestamp',
						Json: 'JSONScalar',
						Decimal: 'BigDecimal',
						Bytes: 'ByteArray',
					},
				}),
			)
			const customGenerator = new UnifiedScalarGenerator(customScalarContext, OutputFormat.GRAPHQL)

			const result = customGenerator.generate()

			expect(result.graphqlTypes).toBeDefined()
			expect(result.graphqlTypes!.length).toBeGreaterThan(0)
		})
	})

	describe('TypeScript Scalar Generation', () => {
		test('should generate TypeScript scalar imports', () => {
			const typescriptGenerator = new UnifiedScalarGenerator(context, OutputFormat.TYPE_GRAPHQL)

			const result = typescriptGenerator.generate()

			expect(result.typescriptTypes).toBeDefined()
			expect(result.typescriptTypes!.length).toBeGreaterThan(0)
		})

		test('should handle GraphQL scalars library imports', () => {
			const typescriptGenerator = new UnifiedScalarGenerator(context, OutputFormat.TYPE_GRAPHQL)

			const result = typescriptGenerator.generate()

			expect(result.typescriptTypes).toBeDefined()
		})
	})

	describe('Error Handling', () => {
		test('should handle invalid scalar configurations gracefully', () => {
			const invalidContext = TestUtils.createMockContext({
				generateScalars: true,
				scalarTypes: {},
			})
			const invalidGenerator = new UnifiedScalarGenerator(invalidContext, OutputFormat.GRAPHQL)

			expect(() => {
				const result = invalidGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})

		test('should handle missing scalar types gracefully', () => {
			const minimalContext = TestUtils.createMockContext({
				generateScalars: true,
			})
			const minimalGenerator = new UnifiedScalarGenerator(minimalContext, OutputFormat.GRAPHQL)

			expect(() => {
				const result = minimalGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})
	})

	describe('Generation Results', () => {
		test('should return consistent result structure', () => {
			const result = generator.generate()

			expect(result).toHaveProperty('graphqlTypes')
			expect(result).toHaveProperty('typescriptTypes')
			expect(Array.isArray(result.graphqlTypes)).toBe(true)
			expect(Array.isArray(result.typescriptTypes)).toBe(true)
		})

		test('should generate different results for different formats', () => {
			const graphqlResult = generator.generate()
			const typescriptGenerator = new UnifiedScalarGenerator(context, OutputFormat.TYPE_GRAPHQL)
			const typescriptResult = typescriptGenerator.generate()

			expect(graphqlResult).not.toEqual(typescriptResult)
		})

		test('should respect generateScalars flag', () => {
			const enabledContext = MockFactory.createMockGraphQLContext(
				TestUtils.createMockContext({
					generateScalars: true,
				}),
			)
			const disabledContext = MockFactory.createMockGraphQLContext(
				TestUtils.createMockContext({
					generateScalars: false,
				}),
			)

			const enabledGenerator = new UnifiedScalarGenerator(enabledContext, OutputFormat.GRAPHQL)
			const disabledGenerator = new UnifiedScalarGenerator(disabledContext, OutputFormat.GRAPHQL)

			const enabledResult = enabledGenerator.generate()
			const disabledResult = disabledGenerator.generate()

			expect(enabledResult.graphqlTypes!.length).toBeGreaterThan(0)
			expect(disabledResult.graphqlTypes!.length).toBe(0)
		})
	})

	describe('Scalar Type Mappings', () => {
		test('should handle DateTime scalar mapping', () => {
			const dateTimeContext = TestUtils.createMockContext({
				generateScalars: true,
				scalarTypes: {
					DateTime: 'DateTime',
				},
			})
			const dateTimeGenerator = new UnifiedScalarGenerator(dateTimeContext, OutputFormat.GRAPHQL)

			const result = dateTimeGenerator.generate()
			expect(result.graphqlTypes).toBeDefined()
		})

		test('should handle JSON scalar mapping', () => {
			const jsonContext = TestUtils.createMockContext({
				generateScalars: true,
				scalarTypes: {
					Json: 'JSON',
				},
			})
			const jsonGenerator = new UnifiedScalarGenerator(jsonContext, OutputFormat.GRAPHQL)

			const result = jsonGenerator.generate()
			expect(result.graphqlTypes).toBeDefined()
		})

		test('should handle Decimal scalar mapping', () => {
			const decimalContext = TestUtils.createMockContext({
				generateScalars: true,
				scalarTypes: {
					Decimal: 'Decimal',
				},
			})
			const decimalGenerator = new UnifiedScalarGenerator(decimalContext, OutputFormat.GRAPHQL)

			const result = decimalGenerator.generate()
			expect(result.graphqlTypes).toBeDefined()
		})

		test('should handle Bytes scalar mapping', () => {
			const bytesContext = TestUtils.createMockContext({
				generateScalars: true,
				scalarTypes: {
					Bytes: 'String',
				},
			})
			const bytesGenerator = new UnifiedScalarGenerator(bytesContext, OutputFormat.GRAPHQL)

			const result = bytesGenerator.generate()
			expect(result.graphqlTypes).toBeDefined()
		})
	})

	describe('Edge Cases', () => {
		test('should handle empty scalar configuration', () => {
			const emptyContext = TestUtils.createMockContext({
				generateScalars: true,
				scalarTypes: {},
			})
			const emptyGenerator = new UnifiedScalarGenerator(emptyContext, OutputFormat.GRAPHQL)

			expect(() => {
				const result = emptyGenerator.generate()
				expect(result).toBeDefined()
				expect(result.graphqlTypes).toBeDefined()
				expect(result.typescriptTypes).toBeDefined()
			}).not.toThrow()
		})

		test('should handle null scalar types gracefully', () => {
			const nullContext = TestUtils.createMockContext({
				generateScalars: true,
			})

			delete (nullContext.options as any).scalarTypes

			const nullGenerator = new UnifiedScalarGenerator(nullContext, OutputFormat.GRAPHQL)

			expect(() => {
				const result = nullGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})

		test('should handle mixed valid scalar mappings', () => {
			const mixedContext = MockFactory.createMockGraphQLContext(
				TestUtils.createMockContext({
					generateScalars: true,
					scalarTypes: {
						DateTime: 'DateTime',
						Json: 'JSON',
						Decimal: 'Decimal',
						Bytes: 'String',
					},
				}),
			)
			const mixedGenerator = new UnifiedScalarGenerator(mixedContext, OutputFormat.GRAPHQL)

			expect(() => {
				const result = mixedGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})
	})
})
