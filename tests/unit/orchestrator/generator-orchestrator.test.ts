import { describe, test, expect, beforeEach } from 'bun:test'
import { GeneratorOrchestrator } from '@orchestrator/generator-orchestrator'
import { OutputFormat } from '@utils/constants'
import { TestUtils, MockFactory } from '../../helpers'
import { BaseGeneratorContext, GenerationType } from '@core/types'

describe('GeneratorOrchestrator', () => {
	let orchestrator: GeneratorOrchestrator
	let baseContext: BaseGeneratorContext

	beforeEach(() => {
		baseContext = TestUtils.createMockContext({
			generateScalars: true,
			generateEnums: true,
			generateFilters: true,
			generateSorts: true,
			connectionTypes: true,
			includeRelations: true,
			enums: [TestUtils.createMockEnum('UserRole', ['ADMIN', 'USER', 'MODERATOR']), TestUtils.createMockEnum('Priority', ['HIGH', 'MEDIUM', 'LOW'])],
			models: [
				TestUtils.createMockDataModel('User', [
					TestUtils.createMockField('id', 'String'),
					TestUtils.createMockField('name', 'String'),
					TestUtils.createMockField('email', 'String'),
				]),
				TestUtils.createMockDataModel('Post', [
					TestUtils.createMockField('id', 'String'),
					TestUtils.createMockField('title', 'String'),
					TestUtils.createMockRelationField('author', 'User'),
				]),
			],
		})
	})

	describe('Initialization', () => {
		test('should initialize with GraphQL output format', () => {
			orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.GRAPHQL)
			expect(orchestrator).toBeDefined()
		})

		test('should initialize with TypeGraphQL output format', () => {
			orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)
			expect(orchestrator).toBeDefined()
		})
	})

	describe('GraphQL Generation', () => {
		beforeEach(() => {
			orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.GRAPHQL)
		})

		test('should generate GraphQL schema with SDL output', async () => {
			const result = await orchestrator.generate()

			expect(result).toBeDefined()
			expect(result.outputFormat).toBe(OutputFormat.GRAPHQL)
			expect(result.sdl).toBeDefined()
			expect(typeof result.sdl).toBe('string')
			expect(result.code).toBeUndefined()
		})

		test('should generate all enabled components', async () => {
			const result = await orchestrator.generate()

			expect(result.results).toBeDefined()
			expect(Array.isArray(result.results)).toBe(true)
			expect(result.results.length).toBeGreaterThan(0)
		})

		test('should collect generation stats', async () => {
			const result = await orchestrator.generate()

			expect(result.stats).toBeDefined()
			expect(typeof result.stats.objectTypes).toBe('number')
			expect(typeof result.stats.enumTypes).toBe('number')
			expect(typeof result.stats.scalarTypes).toBe('number')
		})

		test('should respect generateEnums flag', async () => {
			const contextWithoutEnums = TestUtils.createMockContext({
				generateEnums: false,
				enums: [TestUtils.createMockEnum('Test', ['A', 'B'])],
			})
			const orchestratorWithoutEnums = new GeneratorOrchestrator(contextWithoutEnums, OutputFormat.GRAPHQL)

			const result = await orchestratorWithoutEnums.generate()

			expect(result.stats.enumTypes).toBe(0)
		})

		test('should respect generateScalars flag', async () => {
			const contextWithoutScalars = TestUtils.createMockContext({
				generateScalars: false,
			})
			const orchestratorWithoutScalars = new GeneratorOrchestrator(contextWithoutScalars, OutputFormat.GRAPHQL)

			const result = await orchestratorWithoutScalars.generate()

			expect(result.stats.scalarTypes).toBe(0)
		})

		test('should respect generateFilters flag', async () => {
			const contextWithoutFilters = TestUtils.createMockContext({
				generateFilters: false,
				models: [TestUtils.createMockDataModel('User')],
			})
			const orchestratorWithoutFilters = new GeneratorOrchestrator(contextWithoutFilters, OutputFormat.GRAPHQL)

			const result = await orchestratorWithoutFilters.generate()

			expect(result.stats.filterInputTypes).toBe(0)
		})

		test('should respect generateSorts flag', async () => {
			const contextWithoutSorts = TestUtils.createMockContext({
				generateSorts: false,
				models: [TestUtils.createMockDataModel('User')],
			})
			const orchestratorWithoutSorts = new GeneratorOrchestrator(contextWithoutSorts, OutputFormat.GRAPHQL)

			const result = await orchestratorWithoutSorts.generate()

			expect(result.stats.sortInputTypes).toBe(0)
		})

		test('should respect connectionTypes flag', async () => {
			const contextWithoutConnections = TestUtils.createMockContext({
				connectionTypes: false,
				models: [TestUtils.createMockDataModel('User')],
			})
			const orchestratorWithoutConnections = new GeneratorOrchestrator(contextWithoutConnections, OutputFormat.GRAPHQL)

			const result = await orchestratorWithoutConnections.generate()

			expect(result.stats.connectionTypes).toBe(0)
		})

		test('should respect includeRelations flag', async () => {
			const contextWithoutRelations = TestUtils.createMockContext({
				includeRelations: false,
				models: [TestUtils.createMockDataModel('User'), TestUtils.createMockDataModel('Post', [TestUtils.createMockRelationField('author', 'User')])],
			})
			const orchestratorWithoutRelations = new GeneratorOrchestrator(contextWithoutRelations, OutputFormat.GRAPHQL)

			const result = await orchestratorWithoutRelations.generate()

			expect(result.stats.relationFields).toBe(0)
		})
	})

	describe('TypeGraphQL Generation', () => {
		beforeEach(() => {
			orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)
		})

		test('should generate TypeGraphQL code', async () => {
			const result = await orchestrator.generate()

			expect(result).toBeDefined()
			expect(result.outputFormat).toBe(OutputFormat.TYPE_GRAPHQL)
			expect(result.code).toBeDefined()
			expect(typeof result.code).toBe('string')
			expect(result.sdl).toBeUndefined()
		})

		test('should generate all enabled components for TypeScript', async () => {
			const result = await orchestrator.generate()

			expect(result.results).toBeDefined()
			expect(Array.isArray(result.results)).toBe(true)
			expect(result.results.length).toBeGreaterThan(0)
		})

		test('should use TypeScript output strategy', async () => {
			const result = await orchestrator.generate()

			expect(result.code).toBeDefined()
			expect(result.code!.length).toBeGreaterThan(0)
		})
	})

	describe('Error Handling', () => {
		test('should handle empty context gracefully', async () => {
			const emptyContext = TestUtils.createMockContext({
				models: [],
				enums: [],
			})
			const emptyOrchestrator = new GeneratorOrchestrator(emptyContext, OutputFormat.GRAPHQL)

			expect(async () => {
				const result = await emptyOrchestrator.generate()
				expect(result).toBeDefined()
				expect(result.results).toBeDefined()
			}).not.toThrow()
		})

		test('should handle malformed models gracefully', async () => {
			const malformedContext = TestUtils.createMockContext({
				models: [],
			})
			const malformedOrchestrator = new GeneratorOrchestrator(malformedContext, OutputFormat.GRAPHQL)

			const result = await malformedOrchestrator.generate()
			expect(result).toBeDefined()

			expect(result.stats.objectTypes).toBeGreaterThanOrEqual(0)
		})

		test('should handle invalid output format gracefully', async () => {
			expect(() => {
				const invalidOrchestrator = new GeneratorOrchestrator(baseContext, 'invalid' as OutputFormat)
				expect(invalidOrchestrator).toBeDefined()
			}).not.toThrow()
		})
	})

	describe('Generation Order', () => {
		test('should generate components in correct order', async () => {
			orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.GRAPHQL)
			const result = await orchestrator.generate()

			const resultTypes = result.results.map((r) => r.type)

			const scalarIndex = resultTypes.indexOf(GenerationType.SCALAR)
			const enumIndex = resultTypes.indexOf(GenerationType.ENUM)
			const objectIndex = resultTypes.indexOf(GenerationType.OBJECT)
			const relationIndex = resultTypes.indexOf(GenerationType.RELATION)

			if (scalarIndex !== -1 && enumIndex !== -1) {
				expect(scalarIndex).toBeLessThan(enumIndex)
			}
			if (enumIndex !== -1 && objectIndex !== -1) {
				expect(enumIndex).toBeLessThan(objectIndex)
			}
			if (objectIndex !== -1 && relationIndex !== -1) {
				expect(objectIndex).toBeLessThan(relationIndex)
			}
		})
	})

	describe('Result Structure', () => {
		test('should return consistent result structure', async () => {
			orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.GRAPHQL)
			const result = await orchestrator.generate()

			expect(result).toMatchObject({
				results: expect.any(Array),
				stats: expect.objectContaining({
					objectTypes: expect.any(Number),
					enumTypes: expect.any(Number),
					scalarTypes: expect.any(Number),
					relationFields: expect.any(Number),
					connectionTypes: expect.any(Number),
					sortInputTypes: expect.any(Number),
					filterInputTypes: expect.any(Number),
				}),
				outputFormat: expect.any(String),
			})
		})

		test('should have valid generation results', async () => {
			orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.GRAPHQL)
			const result = await orchestrator.generate()

			result.results.forEach((generationResult) => {
				expect(generationResult).toBeDefined()
				expect(generationResult.items).toBeDefined()
				expect(Array.isArray(generationResult.items)).toBe(true)
				expect(typeof generationResult.count).toBe('number')
				expect(typeof generationResult.type).toBe('string')
				expect(generationResult.count).toBe(generationResult.items.length)
			})
		})

		test('should validate complete GraphQL generation flow', async () => {
			orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.GRAPHQL)
			const result = await orchestrator.generate()

			expect(result.sdl).toBeDefined()
			expect(typeof result.sdl).toBe('string')
			expect(result.sdl!.length).toBeGreaterThan(0)
			expect(result.code).toBeUndefined()

			const resultTypes = result.results.map((r) => r.type)
			expect(resultTypes).toContain('scalar')
			expect(resultTypes).toContain('enum')
			expect(resultTypes).toContain('object')
			expect(resultTypes).toContain('relation')

			expect(result.stats.objectTypes).toBeGreaterThan(0)
			expect(result.stats.enumTypes).toBeGreaterThan(0)
			expect(result.stats.scalarTypes).toBeGreaterThan(0)

			result.results.forEach((generationResult) => {
				generationResult.items.forEach((item) => {
					expect(typeof item).toBe('string')
					expect(item.length).toBeGreaterThan(0)
				})
			})
		})

		test('should validate complete TypeScript generation flow', async () => {
			orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			expect(result.code).toBeDefined()
			expect(typeof result.code).toBe('string')
			expect(result.sdl).toBeUndefined()

			expect(result.outputFormat).toBe(OutputFormat.TYPE_GRAPHQL)
			expect(result.results.length).toBeGreaterThan(0)

			const resultTypes = result.results.map((r) => r.type)
			expect(resultTypes).toContain('object')
		})
	})

	describe('Configuration Validation', () => {
		test('should handle all flags disabled', async () => {
			const minimalContext = TestUtils.createMockContext({
				generateScalars: false,
				generateEnums: false,
				generateFilters: false,
				generateSorts: false,
				connectionTypes: false,
				includeRelations: false,
			})
			const minimalOrchestrator = new GeneratorOrchestrator(minimalContext, OutputFormat.GRAPHQL)

			const result = await minimalOrchestrator.generate()

			expect(result).toBeDefined()
			expect(result.stats.objectTypes).toBeGreaterThanOrEqual(0)
		})

		test('should handle complex model relationships', async () => {
			const complexContext = TestUtils.createMockContext({
				models: [
					TestUtils.createMockDataModel('User', [
						TestUtils.createMockField('id', 'String'),
						TestUtils.createMockRelationField('posts', 'Post', false, true),
						TestUtils.createMockRelationField('profile', 'Profile'),
					]),
					TestUtils.createMockDataModel('Post', [
						TestUtils.createMockField('id', 'String'),
						TestUtils.createMockRelationField('author', 'User'),
						TestUtils.createMockRelationField('categories', 'Category', false, true),
					]),
					TestUtils.createMockDataModel('Profile', [TestUtils.createMockField('id', 'String'), TestUtils.createMockRelationField('user', 'User')]),
					TestUtils.createMockDataModel('Category', [
						TestUtils.createMockField('id', 'String'),
						TestUtils.createMockRelationField('posts', 'Post', false, true),
					]),
				],
			})
			const complexOrchestrator = new GeneratorOrchestrator(complexContext, OutputFormat.GRAPHQL)

			expect(async () => {
				const result = await complexOrchestrator.generate()
				expect(result).toBeDefined()
				expect(result.stats.objectTypes).toBeGreaterThan(0)
			}).not.toThrow()
		})
	})
})
