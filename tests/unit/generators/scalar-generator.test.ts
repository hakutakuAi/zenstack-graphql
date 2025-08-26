import { describe, it, expect, beforeEach } from 'bun:test'
import { UnifiedScalarGenerator } from '@generators/unified/unified-scalar-generator'
import { OutputFormat } from '@utils/constants'
import { TestFixtures, TestMockFactory } from '../../helpers'
import { BaseGeneratorContext } from '@core/types'

describe('Unified Scalar Generator', () => {
	let context: any
	let generator: UnifiedScalarGenerator

	beforeEach(() => {
		context = TestMockFactory.createGraphQLContext(
			TestFixtures.createContext({
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
		it('should initialize with GraphQL format', () => {
			const graphqlGenerator = new UnifiedScalarGenerator(context, OutputFormat.GRAPHQL)
			expect(graphqlGenerator).toBeDefined()
		})

		it('should initialize with TypeScript format', () => {
			const typescriptGenerator = new UnifiedScalarGenerator(context, OutputFormat.TYPE_GRAPHQL)
			expect(typescriptGenerator).toBeDefined()
		})
	})

	describe('Scalar Generation', () => {
		it('should generate default scalars when enabled', () => {
			const result = generator.generate()

			expect(result).toBeDefined()
			expect(result.graphqlTypes).toBeDefined()
			expect(result.graphqlTypes!.length).toBeGreaterThan(0)
		})

		it('should not generate scalars when disabled', () => {
			const disabledContext = TestFixtures.createContext({
				generateScalars: false,
			})
			const disabledGenerator = new UnifiedScalarGenerator(disabledContext, OutputFormat.GRAPHQL)

			const result = disabledGenerator.generate()

			expect(result.graphqlTypes).toEqual([])
		})

		it('should use custom scalar mappings', () => {
			const customContext = TestFixtures.createContext({
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
		it('should generate GraphQL scalar definitions', () => {
			const result = generator.generate()

			expect(result.graphqlTypes).toBeDefined()
			expect(result.graphqlTypes!.length).toBeGreaterThan(0)
		})

		it('should include standard scalars', () => {
			const result = generator.generate()

			expect(result.graphqlTypes).toBeDefined()
			expect(result.graphqlTypes!.length).toBeGreaterThanOrEqual(3)
		})

		it('should respect scalar type configuration', () => {
			const customScalarContext = TestMockFactory.createGraphQLContext(
				TestFixtures.createContext({
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
		it('should generate TypeScript scalar imports', () => {
			const typescriptGenerator = new UnifiedScalarGenerator(context, OutputFormat.TYPE_GRAPHQL)

			const result = typescriptGenerator.generate()

			expect(result.typescriptTypes).toBeDefined()
			expect(result.typescriptTypes!.length).toBeGreaterThan(0)
		})

		it('should handle GraphQL scalars library imports', () => {
			const typescriptGenerator = new UnifiedScalarGenerator(context, OutputFormat.TYPE_GRAPHQL)

			const result = typescriptGenerator.generate()

			expect(result.typescriptTypes).toBeDefined()
		})
	})

	describe('Error Handling', () => {
		it('should handle invalid scalar configurations gracefully', () => {
			const invalidContext = TestFixtures.createContext({
				generateScalars: true,
				scalarTypes: {},
			})
			const invalidGenerator = new UnifiedScalarGenerator(invalidContext, OutputFormat.GRAPHQL)

			expect(() => {
				const result = invalidGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})

		it('should handle missing scalar types gracefully', () => {
			const minimalContext = TestFixtures.createContext({
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
		it('should return consistent result structure', () => {
			const result = generator.generate()

			expect(result).toHaveProperty('graphqlTypes')
			expect(result).toHaveProperty('typescriptTypes')
			expect(Array.isArray(result.graphqlTypes)).toBe(true)
			expect(Array.isArray(result.typescriptTypes)).toBe(true)
		})

		it('should generate different results for different formats', () => {
			const graphqlResult = generator.generate()
			const typescriptGenerator = new UnifiedScalarGenerator(context, OutputFormat.TYPE_GRAPHQL)
			const typescriptResult = typescriptGenerator.generate()

			expect(graphqlResult).not.toEqual(typescriptResult)
		})

		it('should respect generateScalars flag', () => {
			const enabledContext = TestMockFactory.createGraphQLContext(
				TestFixtures.createContext({
					generateScalars: true,
				}),
			)
			const disabledContext = TestMockFactory.createGraphQLContext(
				TestFixtures.createContext({
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
		it('should handle DateTime scalar mapping', () => {
			const dateTimeContext = TestFixtures.createContext({
				generateScalars: true,
				scalarTypes: {
					DateTime: 'DateTime',
				},
			})
			const dateTimeGenerator = new UnifiedScalarGenerator(dateTimeContext, OutputFormat.GRAPHQL)

			const result = dateTimeGenerator.generate()
			expect(result.graphqlTypes).toBeDefined()
		})

		it('should handle JSON scalar mapping', () => {
			const jsonContext = TestFixtures.createContext({
				generateScalars: true,
				scalarTypes: {
					Json: 'JSON',
				},
			})
			const jsonGenerator = new UnifiedScalarGenerator(jsonContext, OutputFormat.GRAPHQL)

			const result = jsonGenerator.generate()
			expect(result.graphqlTypes).toBeDefined()
		})

		it('should handle Decimal scalar mapping', () => {
			const decimalContext = TestFixtures.createContext({
				generateScalars: true,
				scalarTypes: {
					Decimal: 'Decimal',
				},
			})
			const decimalGenerator = new UnifiedScalarGenerator(decimalContext, OutputFormat.GRAPHQL)

			const result = decimalGenerator.generate()
			expect(result.graphqlTypes).toBeDefined()
		})

		it('should handle Bytes scalar mapping', () => {
			const bytesContext = TestFixtures.createContext({
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
		it('should handle empty scalar configuration', () => {
			const emptyContext = TestFixtures.createContext({
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

		it('should handle null scalar types gracefully', () => {
			const nullContext = TestFixtures.createContext({
				generateScalars: true,
			})

			delete (nullContext.options as any).scalarTypes

			const nullGenerator = new UnifiedScalarGenerator(nullContext, OutputFormat.GRAPHQL)

			expect(() => {
				const result = nullGenerator.generate()
				expect(result).toBeDefined()
			}).not.toThrow()
		})

		it('should handle mixed valid scalar mappings', () => {
			const mixedContext = TestMockFactory.createGraphQLContext(
				TestFixtures.createContext({
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
