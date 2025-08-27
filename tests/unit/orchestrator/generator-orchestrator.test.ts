import { describe, it, expect, beforeEach } from 'bun:test'
import { GeneratorOrchestrator } from '@orchestrator/generator-orchestrator'
import { OutputFormat } from '@utils/constants'
import { TestFixtures } from '../../helpers'
import { BaseGeneratorContext, GenerationType } from '@core/types'

describe('GeneratorOrchestrator', () => {
	let orchestrator: GeneratorOrchestrator
	let baseContext: BaseGeneratorContext

	beforeEach(() => {
		baseContext = TestFixtures.createContext({
			generateScalars: true,
			generateEnums: true,
			generateFilters: true,
			generateSorts: true,
			connectionTypes: true,
			includeRelations: true,
			enums: [TestFixtures.createEnum('UserRole', ['ADMIN', 'USER', 'MODERATOR']), TestFixtures.createEnum('Priority', ['HIGH', 'MEDIUM', 'LOW'])],
			models: [
				TestFixtures.createDataModel('User', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('name', 'String'),
					TestFixtures.createField('email', 'String'),
				]),
				TestFixtures.createDataModel('Post', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('title', 'String'),
					TestFixtures.createRelationField('author', 'User'),
				]),
			],
		})
	})

	describe('Initialization', () => {
		it('should initialize with GraphQL output format', () => {
			orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.GRAPHQL)
			expect(orchestrator).toBeDefined()
		})

		it('should initialize with TypeGraphQL output format', () => {
			orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)
			expect(orchestrator).toBeDefined()
		})
	})

	describe('GraphQL Generation', () => {
		beforeEach(() => {
			orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.GRAPHQL)
		})

		it('should generate GraphQL schema with SDL output', async () => {
			const result = await orchestrator.generate()

			expect(result).toBeDefined()
			expect(result.outputFormat).toBe(OutputFormat.GRAPHQL)
			expect(result.sdl).toBeDefined()
			expect(typeof result.sdl).toBe('string')
			expect(result.code).toBeUndefined()
		})

		it('should generate all enabled components', async () => {
			const result = await orchestrator.generate()

			expect(result.results).toBeDefined()
			expect(Array.isArray(result.results)).toBe(true)
			expect(result.results.length).toBeGreaterThan(0)
		})

		it('should collect generation stats', async () => {
			const result = await orchestrator.generate()

			expect(result.stats).toBeDefined()
			expect(typeof result.stats.objectTypes).toBe('number')
			expect(typeof result.stats.enumTypes).toBe('number')
			expect(typeof result.stats.scalarTypes).toBe('number')
		})

		it('should respect generateEnums flag', async () => {
			const contextWithoutEnums = TestFixtures.createContext({
				generateEnums: false,
				enums: [TestFixtures.createEnum('Test', ['A', 'B'])],
			})
			const orchestratorWithoutEnums = new GeneratorOrchestrator(contextWithoutEnums, OutputFormat.GRAPHQL)

			const result = await orchestratorWithoutEnums.generate()

			expect(result.stats.enumTypes).toBe(0)
		})

		it('should respect generateScalars flag', async () => {
			const contextWithoutScalars = TestFixtures.createContext({
				generateScalars: false,
			})
			const orchestratorWithoutScalars = new GeneratorOrchestrator(contextWithoutScalars, OutputFormat.GRAPHQL)

			const result = await orchestratorWithoutScalars.generate()

			expect(result.stats.scalarTypes).toBe(0)
		})

		it('should respect generateFilters flag', async () => {
			const contextWithoutFilters = TestFixtures.createContext({
				generateFilters: false,
				models: [TestFixtures.createDataModel('User')],
			})
			const orchestratorWithoutFilters = new GeneratorOrchestrator(contextWithoutFilters, OutputFormat.GRAPHQL)

			const result = await orchestratorWithoutFilters.generate()

			expect(result.stats.filterInputTypes).toBe(0)
		})

		it('should respect generateSorts flag', async () => {
			const contextWithoutSorts = TestFixtures.createContext({
				generateSorts: false,
				models: [TestFixtures.createDataModel('User')],
			})
			const orchestratorWithoutSorts = new GeneratorOrchestrator(contextWithoutSorts, OutputFormat.GRAPHQL)

			const result = await orchestratorWithoutSorts.generate()

			expect(result.stats.sortInputTypes).toBe(0)
		})

		it('should respect connectionTypes flag', async () => {
			const contextWithoutConnections = TestFixtures.createContext({
				connectionTypes: false,
				models: [TestFixtures.createDataModel('User')],
			})
			const orchestratorWithoutConnections = new GeneratorOrchestrator(contextWithoutConnections, OutputFormat.GRAPHQL)

			const result = await orchestratorWithoutConnections.generate()

			expect(result.stats.connectionTypes).toBe(0)
		})

		it('should respect includeRelations flag', async () => {
			const contextWithoutRelations = TestFixtures.createContext({
				includeRelations: false,
				models: [TestFixtures.createDataModel('User'), TestFixtures.createDataModel('Post', [TestFixtures.createRelationField('author', 'User')])],
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

		it('should generate TypeGraphQL code', async () => {
			const result = await orchestrator.generate()

			expect(result).toBeDefined()
			expect(result.outputFormat).toBe(OutputFormat.TYPE_GRAPHQL)
			expect(result.code).toBeDefined()
			expect(typeof result.code).toBe('string')
			expect(result.sdl).toBeUndefined()
		})

		it('should generate all enabled components for TypeScript', async () => {
			const result = await orchestrator.generate()

			expect(result.results).toBeDefined()
			expect(Array.isArray(result.results)).toBe(true)
			expect(result.results.length).toBeGreaterThan(0)
		})

		it('should use TypeScript output strategy', async () => {
			const result = await orchestrator.generate()

			expect(result.code).toBeDefined()
			expect(result.code!.length).toBeGreaterThan(0)
		})
	})

	describe('Error Handling', () => {
		it('should handle empty context gracefully', async () => {
			const emptyContext = TestFixtures.createContext({
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

		it('should handle malformed models gracefully', async () => {
			const malformedContext = TestFixtures.createContext({
				models: [],
			})
			const malformedOrchestrator = new GeneratorOrchestrator(malformedContext, OutputFormat.GRAPHQL)

			const result = await malformedOrchestrator.generate()
			expect(result).toBeDefined()

			expect(result.stats.objectTypes).toBeGreaterThanOrEqual(0)
		})

		it('should handle invalid output format gracefully', async () => {
			expect(() => {
				const invalidOrchestrator = new GeneratorOrchestrator(baseContext, 'invalid' as OutputFormat)
				expect(invalidOrchestrator).toBeDefined()
			}).not.toThrow()
		})
	})

	describe('Generation Order', () => {
		it('should generate components in correct order', async () => {
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
		it('should return consistent result structure', async () => {
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

		it('should have valid generation results', async () => {
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

		it('should validate complete GraphQL generation flow', async () => {
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

		it('should validate complete TypeScript generation flow', async () => {
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
		it('should handle all flags disabled', async () => {
			const minimalContext = TestFixtures.createContext({
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

		it('should handle complex model relationships', async () => {
			const complexContext = TestFixtures.createContext({
				models: [
					TestFixtures.createDataModel('User', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createRelationField('posts', 'Post', false, true),
						TestFixtures.createRelationField('profile', 'Profile'),
					]),
					TestFixtures.createDataModel('Post', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createRelationField('author', 'User'),
						TestFixtures.createRelationField('categories', 'Category', false, true),
					]),
					TestFixtures.createDataModel('Profile', [TestFixtures.createField('id', 'String'), TestFixtures.createRelationField('user', 'User')]),
					TestFixtures.createDataModel('Category', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createRelationField('posts', 'Post', false, true),
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

	describe('Helper Generation Tests', () => {
		beforeEach(() => {
			baseContext = TestFixtures.createContext({
				generateHelpers: true,
				generateFilters: true,
				generateSorts: true,
				connectionTypes: true,
				includeRelations: true,
				generateScalars: true,
				generateEnums: true,
				models: [
					TestFixtures.createDataModel('User', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('name', 'String'),
						TestFixtures.createField('email', 'String'),
					]),
					TestFixtures.createDataModel('Post', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('title', 'String'),
						TestFixtures.createRelationField('author', 'User'),
					]),
				],
			})
		})

		it('should NOT generate helpers for GraphQL output format', async () => {
			orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.GRAPHQL)
			const result = await orchestrator.generate()

			expect(result).toBeDefined()
			expect(result.outputFormat).toBe(OutputFormat.GRAPHQL)
			
			// Helper result should not be present
			const helperResult = result.results.find(r => r.type === GenerationType.HELPER)
			expect(helperResult).toBeUndefined()
			
			// Helper code should not be present
			expect(result.helperCode).toBeUndefined()
			
			// SDL should be present for GraphQL
			expect(result.sdl).toBeDefined()
			expect(typeof result.sdl).toBe('string')
			expect(result.code).toBeUndefined()
		})

		it('should generate helpers for TypeGraphQL output format', async () => {
			orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			expect(result).toBeDefined()
			expect(result.outputFormat).toBe(OutputFormat.TYPE_GRAPHQL)
			
			// Helper result should be present
			const helperResult = result.results.find(r => r.type === GenerationType.HELPER)
			expect(helperResult).toBeDefined()
			expect(helperResult!.items).toBeDefined()
			expect(helperResult!.items.length).toBeGreaterThan(0)
			
			// Helper code should be present in result
			expect(result.helperCode).toBeDefined()
			expect(typeof result.helperCode).toBe('string')
			expect(result.helperCode!.length).toBeGreaterThan(0)
			
			// TypeScript code should be present
			expect(result.code).toBeDefined()
			expect(typeof result.code).toBe('string')
			expect(result.sdl).toBeUndefined()
		})

		it('should respect generateHelpers flag when false', async () => {
			const contextWithoutHelpers = TestFixtures.createContext({
				...baseContext.options,
				generateHelpers: false,
				models: baseContext.models,
			})
			orchestrator = new GeneratorOrchestrator(contextWithoutHelpers, OutputFormat.TYPE_GRAPHQL)

			const result = await orchestrator.generate()

			expect(result).toBeDefined()
			
			// Helper result should not be present even in TypeGraphQL format
			const helperResult = result.results.find(r => r.type === GenerationType.HELPER)
			expect(helperResult).toBeUndefined()
			
			// Helper code should not be present
			expect(result.helperCode).toBeUndefined()
		})

		it('should generate helper content with connection builders', async () => {
			orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			expect(result.helperCode).toBeDefined()
			const helperCode = result.helperCode!

			// Check for connection builder content
			expect(helperCode).toContain('ConnectionBuilder')
			expect(helperCode).toContain('buildUserConnectionConfig')
			expect(helperCode).toContain('buildPostConnectionConfig')
			
			// Check for filter builder content
			expect(helperCode).toContain('FilterBuilder')
			
			// Check for sort builder content
			expect(helperCode).toContain('SortBuilder')
			
			// Check for field selection content
			expect(helperCode).toContain('FieldSelection')
			
			// Check for includes content
			expect(helperCode).toContain('USER_INCLUDES')
			expect(helperCode).toContain('POST_INCLUDES')
		})

		it('should generate helper content with correct imports', async () => {
			orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			expect(result.helperCode).toBeDefined()
			const helperCode = result.helperCode!

			// Check for GraphQL imports
			expect(helperCode).toContain('import type { GraphQLResolveInfo } from \'graphql\'')
			
			// Check for schema imports
			expect(helperCode).toContain('import {')
			expect(helperCode).toContain('} from \'./schema\'')
			
			// Check for type imports
			expect(helperCode).toContain('User')
			expect(helperCode).toContain('UserQueryArgs')
			expect(helperCode).toContain('UserConnection')
			expect(helperCode).toContain('Post')
			expect(helperCode).toContain('PostQueryArgs')
			expect(helperCode).toContain('PostConnection')
		})

		it('should include helper interfaces in generated code', async () => {
			orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			expect(result.helperCode).toBeDefined()
			const helperCode = result.helperCode!

			// Check for helper interfaces
			expect(helperCode).toContain('export interface PaginationArgs')
			expect(helperCode).toContain('export interface ConnectionResult<T>')
			expect(helperCode).toContain('export interface ConnectionConfig')
			
			// Check pagination args fields
			expect(helperCode).toContain('first?: number')
			expect(helperCode).toContain('after?: string')
			expect(helperCode).toContain('last?: number')
			expect(helperCode).toContain('before?: string')
		})

		it('should handle models without relations in helpers', async () => {
			const contextWithoutRelations = TestFixtures.createContext({
				generateHelpers: true,
				models: [
					TestFixtures.createDataModel('SimpleUser', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('name', 'String'),
					]),
				],
			})
			orchestrator = new GeneratorOrchestrator(contextWithoutRelations, OutputFormat.TYPE_GRAPHQL)

			const result = await orchestrator.generate()

			expect(result.helperCode).toBeDefined()
			const helperCode = result.helperCode!

			expect(helperCode).toContain('buildSimpleUserConnectionConfig')
			expect(helperCode).toContain('SIMPLEUSER_INCLUDES')
		})

		it('should maintain consistency between GraphQL and TypeGraphQL generations', async () => {
			// Generate both formats
			const graphqlOrchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.GRAPHQL)
			const typeGraphQLOrchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)

			const graphqlResult = await graphqlOrchestrator.generate()
			const typeGraphQLResult = await typeGraphQLOrchestrator.generate()

			// Both should succeed
			expect(graphqlResult).toBeDefined()
			expect(typeGraphQLResult).toBeDefined()

			// Both should have similar stats counts (except helpers)
			expect(graphqlResult.stats.objectTypes).toBe(typeGraphQLResult.stats.objectTypes)
			expect(graphqlResult.stats.enumTypes).toBe(typeGraphQLResult.stats.enumTypes)
			expect(graphqlResult.stats.scalarTypes).toBe(typeGraphQLResult.stats.scalarTypes)

			// Only TypeGraphQL should have helpers
			expect(graphqlResult.helperCode).toBeUndefined()
			expect(typeGraphQLResult.helperCode).toBeDefined()
		})
	})
})
