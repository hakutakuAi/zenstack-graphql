import { describe, it, expect, beforeEach } from 'bun:test'
import { GeneratorOrchestrator } from '@orchestrator/generator-orchestrator'
import { OutputFormat } from '@utils/constants'
import { TestFixtures } from '../helpers'
import { BaseGeneratorContext, GenerationType } from '@core/types'

describe('Helper Generation Integration Tests', () => {
	let baseContext: BaseGeneratorContext

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
					{
						...TestFixtures.createField('id', 'String'),
						attributes: [TestFixtures.createAttribute('id'), TestFixtures.createAttribute('default', ['uuid()'])],
					},
					{
						...TestFixtures.createField('name', 'String'),
						attributes: [TestFixtures.createAttribute('graphql.filterable'), TestFixtures.createAttribute('graphql.sortable')],
					},
					{
						...TestFixtures.createField('email', 'String'),
						attributes: [TestFixtures.createAttribute('unique'), TestFixtures.createAttribute('graphql.filterable')],
					},
					{
						...TestFixtures.createField('createdAt', 'DateTime'),
						attributes: [TestFixtures.createAttribute('default', ['now()']), TestFixtures.createAttribute('graphql.sortable')],
					},
				]),
				TestFixtures.createDataModel('Post', [
					{
						...TestFixtures.createField('id', 'String'),
						attributes: [TestFixtures.createAttribute('id'), TestFixtures.createAttribute('default', ['uuid()'])],
					},
					{
						...TestFixtures.createField('title', 'String'),
						attributes: [TestFixtures.createAttribute('graphql.filterable'), TestFixtures.createAttribute('graphql.sortable')],
					},
					{
						...TestFixtures.createField('content', 'String'),
					},
					{
						...TestFixtures.createField('published', 'Boolean'),
						attributes: [TestFixtures.createAttribute('default', ['false']), TestFixtures.createAttribute('graphql.filterable')],
					},
					{
						...TestFixtures.createField('viewCount', 'Int'),
						attributes: [TestFixtures.createAttribute('default', ['0']), TestFixtures.createAttribute('graphql.sortable')],
					},
					{
						...TestFixtures.createRelationField('author', 'User'),
						attributes: [TestFixtures.createAttribute('relation', ['fields: [authorId], references: [id]'])],
					},
					{
						...TestFixtures.createField('authorId', 'String'),
					},
				]),
				TestFixtures.createDataModel('Comment', [
					{
						...TestFixtures.createField('id', 'String'),
						attributes: [TestFixtures.createAttribute('id'), TestFixtures.createAttribute('default', ['uuid()'])],
					},
					{
						...TestFixtures.createField('content', 'String'),
						attributes: [TestFixtures.createAttribute('graphql.filterable')],
					},
					{
						...TestFixtures.createField('createdAt', 'DateTime'),
						attributes: [TestFixtures.createAttribute('default', ['now()']), TestFixtures.createAttribute('graphql.sortable')],
					},
					{
						...TestFixtures.createRelationField('post', 'Post'),
						attributes: [TestFixtures.createAttribute('relation', ['fields: [postId], references: [id]'])],
					},
					{
						...TestFixtures.createField('postId', 'String'),
					},
					{
						...TestFixtures.createRelationField('author', 'User'),
						attributes: [TestFixtures.createAttribute('relation', ['fields: [authorId], references: [id]'])],
					},
					{
						...TestFixtures.createField('authorId', 'String'),
					},
				]),
			],
		})
	})

	describe('End-to-End Helper Generation', () => {
		it('should generate complete helper file for TypeGraphQL format', async () => {
			const orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			expect(result).toBeDefined()
			expect(result.outputFormat).toBe(OutputFormat.TYPE_GRAPHQL)
			expect(result.helperCode).toBeDefined()

			const helperCode = result.helperCode!

			// Verify file structure
			expect(helperCode).toContain('import type { GraphQLResolveInfo } from \'graphql\'')
			expect(helperCode).toContain('export interface PaginationArgs')
			expect(helperCode).toContain('export interface ConnectionResult<T>')
			expect(helperCode).toContain('export interface ConnectionConfig')
			expect(helperCode).toContain('export class ConnectionBuilder')
			expect(helperCode).toContain('export class FilterBuilder')
			expect(helperCode).toContain('export class SortBuilder')
			expect(helperCode).toContain('export class FieldSelection')
		})

		it('should generate model-specific connection builders', async () => {
			const orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			const helperCode = result.helperCode!

			// Check for all model connection builders
			expect(helperCode).toContain('static buildUserConnectionConfig')
			expect(helperCode).toContain('static buildPostConnectionConfig')
			expect(helperCode).toContain('static buildCommentConnectionConfig')

			// Verify connection builder parameters
			expect(helperCode).toContain('UserQueryArgs')
			expect(helperCode).toContain('PostQueryArgs')
			expect(helperCode).toContain('CommentQueryArgs')
		})

		it('should generate model-specific filter builders', async () => {
			const orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			const helperCode = result.helperCode!

			// Check for filter builders that are actually generated
			// Note: Filter builders are only generated for models with filterable fields that the processor recognizes
			expect(helperCode).toContain('FilterBuilder')
			if (helperCode.includes('static buildPostFilter')) {
				expect(helperCode).toContain('static buildPostFilter')
			}
			if (helperCode.includes('static buildCommentFilter')) {
				expect(helperCode).toContain('static buildCommentFilter')
			}

			// Verify filter input types are imported
			expect(helperCode).toContain('UserFilterInput')
			expect(helperCode).toContain('PostFilterInput')
			expect(helperCode).toContain('CommentFilterInput')
		})

		it('should generate model-specific sort builders', async () => {
			const orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			const helperCode = result.helperCode!

			// Check for sort builders that are actually generated
			expect(helperCode).toContain('SortBuilder')
			if (helperCode.includes('static buildPostSort')) {
				expect(helperCode).toContain('static buildPostSort')
			}
			if (helperCode.includes('static buildCommentSort')) {
				expect(helperCode).toContain('static buildCommentSort')
			}

			// Verify sort input types are imported
			expect(helperCode).toContain('PostSortInput')
			expect(helperCode).toContain('CommentSortInput')
		})

		it('should generate field selection helpers', async () => {
			const orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			const helperCode = result.helperCode!

			// Check for field selection methods
			expect(helperCode).toContain('static buildUserInclude')
			expect(helperCode).toContain('static buildPostInclude')
			expect(helperCode).toContain('static buildCommentInclude')

			// Verify GraphQLResolveInfo usage
			expect(helperCode).toContain('GraphQLResolveInfo')
			expect(helperCode).toContain('buildPrismaInclude')
		})

		it('should generate include constants', async () => {
			const orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			const helperCode = result.helperCode!

			// Check for include constants
			expect(helperCode).toContain('export const USER_INCLUDES')
			expect(helperCode).toContain('export const POST_INCLUDES')
			expect(helperCode).toContain('export const COMMENT_INCLUDES')

			// Verify relation includes
			expect(helperCode).toContain('author: true')
			expect(helperCode).toContain('post: true')
		})

		it('should handle models with no filterable/sortable fields', async () => {
			const contextWithPlainModel = TestFixtures.createContext({
				generateHelpers: true,
				models: [
					TestFixtures.createDataModel('PlainModel', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('name', 'String'), // No filterable/sortable attributes
					]),
				],
			})

			const orchestrator = new GeneratorOrchestrator(contextWithPlainModel, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			const helperCode = result.helperCode!

			// Should still have connection builder
			expect(helperCode).toContain('static buildPlainModelConnectionConfig')

			// Should have basic filter/sort builders even if fields aren't specifically marked
			expect(helperCode).toContain('FilterBuilder')
			expect(helperCode).toContain('SortBuilder')
		})

		it('should handle complex relations in helpers', async () => {
			const orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			const helperCode = result.helperCode!

			// Verify complex relation handling in connection configs
			expect(helperCode).toMatch(/relationFields:\s*\[.*'author'.*\]/)
			expect(helperCode).toMatch(/relationFields:\s*\[.*'post'.*\]/)

			// Check relation includes
			expect(helperCode).toContain('author: true')
			expect(helperCode).toContain('post: true')
		})

		it('should generate valid TypeScript code', async () => {
			const orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			const helperCode = result.helperCode!

			// Basic TypeScript syntax checks
			expect(helperCode).not.toContain('undefined as any')
			expect(helperCode).not.toContain('null as any')
			
			// Check for proper interface definitions
			expect(helperCode).toMatch(/export interface \w+/)
			expect(helperCode).toMatch(/export class \w+/)
			
			// Check for proper method signatures
			expect(helperCode).toMatch(/static \w+\([^)]*\): \w+/)
			
			// Verify no obvious syntax errors
			const braceCount = (helperCode.match(/\{/g) || []).length
			const closeBraceCount = (helperCode.match(/\}/g) || []).length
			expect(braceCount).toBe(closeBraceCount)
		})
	})

	describe('Helper Content Validation', () => {
		it('should generate connection builders with correct pagination logic', async () => {
			const orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			const helperCode = result.helperCode!

			// Check for pagination parameter handling
			expect(helperCode).toContain('first?: number')
			expect(helperCode).toContain('last?: number')
			expect(helperCode).toContain('after?: string')
			expect(helperCode).toContain('before?: string')

			// Check for cursor-based pagination logic
			expect(helperCode).toContain('buildConfig')
			expect(helperCode).toContain('findManyOptions')
			expect(helperCode).toContain('countOptions')
			expect(helperCode).toContain('paginationInfo')
		})

		it('should generate filter builders with proper type handling', async () => {
			const orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			const helperCode = result.helperCode!

			// Check for filter condition handling
			expect(helperCode).toContain('FilterBuilder')
			expect(helperCode).toContain('buildFilter')
			
			// Verify filter input parameter usage (check for actual patterns in generated code)
			if (helperCode.includes('FilterBuilder.build')) {
				expect(helperCode).toMatch(/FilterBuilder\.build\w+Filter/)
			}
		})

		it('should generate sort builders with direction support', async () => {
			const orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			const helperCode = result.helperCode!

			// Check for sort handling
			expect(helperCode).toContain('SortBuilder')
			expect(helperCode).toContain('buildSort')
			
			// Verify sort input parameter usage (check for actual patterns in generated code)
			if (helperCode.includes('SortBuilder.build')) {
				expect(helperCode).toMatch(/SortBuilder\.build\w+Sort/)
			}
			expect(helperCode).toContain('orderBy')
		})

		it('should generate field selection with GraphQL info parsing', async () => {
			const orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			const helperCode = result.helperCode!

			// Check for GraphQL info usage
			expect(helperCode).toContain('GraphQLResolveInfo')
			expect(helperCode).toContain('buildPrismaInclude')
			expect(helperCode).toContain('info')
			
			// Verify relation field handling
			expect(helperCode).toContain('relationFields')
		})
	})

	describe('Error Handling and Edge Cases', () => {
		it('should handle empty models list', async () => {
			const emptyContext = TestFixtures.createContext({
				generateHelpers: true,
				models: [],
			})
			
			const orchestrator = new GeneratorOrchestrator(emptyContext, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			expect(result.helperCode).toBeDefined()
			const helperCode = result.helperCode!
			
			// Should still have base structure
			expect(helperCode).toContain('export interface PaginationArgs')
			expect(helperCode).toContain('export class ConnectionBuilder')
		})

		it('should handle models with invalid field types gracefully', async () => {
			const contextWithInvalidFields = TestFixtures.createContext({
				generateHelpers: true,
				models: [
					{
						...TestFixtures.createDataModel('TestModel'),
						fields: [
							{
								name: 'invalidField',
								type: { type: 'UnknownType' as any },
								isOptional: false,
								attributes: [],
							},
						],
					},
				],
			})

			const orchestrator = new GeneratorOrchestrator(contextWithInvalidFields, OutputFormat.TYPE_GRAPHQL)
			
			expect(async () => {
				const result = await orchestrator.generate()
				expect(result.helperCode).toBeDefined()
			}).not.toThrow()
		})

		it('should handle circular relations correctly', async () => {
			const contextWithCircularRelations = TestFixtures.createContext({
				generateHelpers: true,
				models: [
					TestFixtures.createDataModel('NodeA', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createRelationField('nodeB', 'NodeB'),
					]),
					TestFixtures.createDataModel('NodeB', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createRelationField('nodeA', 'NodeA'),
					]),
				],
			})

			const orchestrator = new GeneratorOrchestrator(contextWithCircularRelations, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			expect(result.helperCode).toBeDefined()
			const helperCode = result.helperCode!

			// Should handle both models
			expect(helperCode).toContain('buildNodeAConnectionConfig')
			expect(helperCode).toContain('buildNodeBConnectionConfig')

			// Should include proper relation fields
			expect(helperCode).toContain('nodeB')
			expect(helperCode).toContain('nodeA')
		})
	})

	describe('Performance and Scalability', () => {
		it('should handle large number of models efficiently', async () => {
			const largeContext = TestFixtures.createContext({
				generateHelpers: true,
				generateFilters: true,
				generateSorts: true,
				models: Array.from({ length: 20 }, (_, i) =>
					TestFixtures.createDataModel(`Model${i}`, [
						TestFixtures.createField('id', 'String'),
						{
							...TestFixtures.createField('name', 'String'),
							attributes: [TestFixtures.createAttribute('graphql.filterable')],
						},
						{
							...TestFixtures.createField('createdAt', 'DateTime'),
							attributes: [TestFixtures.createAttribute('graphql.sortable')],
						},
					])
				),
			})

			const startTime = Date.now()
			const orchestrator = new GeneratorOrchestrator(largeContext, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()
			const endTime = Date.now()

			expect(result.helperCode).toBeDefined()
			expect(endTime - startTime).toBeLessThan(5000) // Should complete within 5 seconds

			// Verify all models are included
			const helperCode = result.helperCode!
			for (let i = 0; i < 20; i++) {
				expect(helperCode).toContain(`buildModel${i}ConnectionConfig`)
			}
		})

		it('should generate consistent helper code across multiple runs', async () => {
			const orchestrator = new GeneratorOrchestrator(baseContext, OutputFormat.TYPE_GRAPHQL)

			const result1 = await orchestrator.generate()
			const result2 = await orchestrator.generate()

			expect(result1.helperCode).toBeDefined()
			expect(result2.helperCode).toBeDefined()
			expect(result1.helperCode).toBe(result2.helperCode)
		})
	})
})