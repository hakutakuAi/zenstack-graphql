import { describe, it, expect, beforeEach } from 'bun:test'
import { SchemaBuilder } from '../helpers'
import { GeneratorOrchestrator } from '@orchestrator/generator-orchestrator'
import { OutputFormat } from '@utils/constants'
import { TestFixtures } from '../helpers'
import { BaseGeneratorContext } from '@core/types'

describe('End-to-End Integration Tests', () => {
	let schemaBuilder: SchemaBuilder

	beforeEach(() => {
		schemaBuilder = new SchemaBuilder()
	})

	describe('Complex Multi-Model Schema', () => {
		it('should generate complete GraphQL schema for complex e-commerce scenario', async () => {
			const context: BaseGeneratorContext = TestFixtures.createContext({
				outputFormat: OutputFormat.GRAPHQL,
				generateEnums: true,
				generateScalars: true,
				generateFilters: true,
				generateSorts: true,
				connectionTypes: true,
				includeRelations: true,
				models: [
					TestFixtures.createDataModel('User', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('email', 'String'),
						TestFixtures.createField('name', 'String', true),
						TestFixtures.createField('createdAt', 'DateTime'),
						TestFixtures.createField('updatedAt', 'DateTime'),
					]),
					TestFixtures.createDataModel('Profile', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('bio', 'String', true),
						TestFixtures.createField('avatar', 'String', true),
						TestFixtures.createField('phone', 'String', true),
						TestFixtures.createRelationField('user', 'User'),
					]),
					TestFixtures.createDataModel('Category', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('name', 'String'),
						TestFixtures.createField('description', 'String', true),
						TestFixtures.createField('createdAt', 'DateTime'),
					]),
					TestFixtures.createDataModel('Product', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('name', 'String'),
						TestFixtures.createField('description', 'String', true),
						TestFixtures.createField('price', 'Decimal'),
						TestFixtures.createField('stock', 'Int'),
						TestFixtures.createField('active', 'Boolean'),
						TestFixtures.createField('tags', 'Json', true),
						TestFixtures.createField('createdAt', 'DateTime'),
						TestFixtures.createField('updatedAt', 'DateTime'),
						TestFixtures.createRelationField('category', 'Category'),
					]),
					TestFixtures.createDataModel('Order', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('total', 'Decimal'),
						TestFixtures.createField('createdAt', 'DateTime'),
						TestFixtures.createRelationField('user', 'User'),
					]),
					TestFixtures.createDataModel('Review', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('rating', 'Int'),
						TestFixtures.createField('comment', 'String', true),
						TestFixtures.createField('createdAt', 'DateTime'),
						TestFixtures.createRelationField('user', 'User'),
						TestFixtures.createRelationField('product', 'Product'),
					]),
				],
				enums: [TestFixtures.createEnum('OrderStatus', ['PENDING', 'CONFIRMED', 'SHIPPED', 'DELIVERED', 'CANCELLED'])],
			})

			const orchestrator = new GeneratorOrchestrator(context, OutputFormat.GRAPHQL)
			const result = await orchestrator.generate()

			expect(result).toBeDefined()
			expect(result.sdl).toBeDefined()
			expect(result.sdl).toContain('type User')
			expect(result.sdl).toContain('type Product')
			expect(result.sdl).toContain('type Order')
			expect(result.sdl).toContain('type Review')
			expect(result.sdl).toContain('enum OrderStatus')
			expect(result.sdl).toContain('input UserFilterInput')
			expect(result.sdl).toContain('input ProductSortInput')
			expect(result.sdl).toContain('type UserConnection')
			expect(result.sdl).toContain('scalar DateTime')
			expect(result.sdl).toContain('scalar Decimal')
			expect(result.sdl).toContain('scalar JSON')

			expect(result.stats.objectTypes).toBeGreaterThanOrEqual(6)
			expect(result.stats.inputTypes).toBeGreaterThan(15)
			expect(result.stats.enumTypes).toBeGreaterThan(0)
			expect(result.stats.scalarTypes).toBeGreaterThan(0)
		})

		it('should generate complete TypeScript code for complex scenario', async () => {
			const context: BaseGeneratorContext = TestFixtures.createContext({
				outputFormat: OutputFormat.TYPE_GRAPHQL,
				generateEnums: true,
				generateScalars: true,
				generateFilters: true,
				generateSorts: true,
				connectionTypes: true,
				includeRelations: true,
				models: [
					TestFixtures.createDataModel('User', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('email', 'String'),
						TestFixtures.createField('name', 'String', true),
					]),
					TestFixtures.createDataModel('Profile', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('bio', 'String', true),
						TestFixtures.createRelationField('user', 'User'),
					]),
					TestFixtures.createDataModel('Post', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('title', 'String'),
						TestFixtures.createField('content', 'String', true),
						TestFixtures.createField('published', 'Boolean'),
						TestFixtures.createField('tags', 'Json', true),
						TestFixtures.createField('metadata', 'Json', true),
						TestFixtures.createRelationField('author', 'User'),
					]),
					TestFixtures.createDataModel('Comment', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('content', 'String'),
						TestFixtures.createRelationField('post', 'Post'),
						TestFixtures.createRelationField('author', 'User'),
					]),
				],
			})

			const orchestrator = new GeneratorOrchestrator(context, OutputFormat.TYPE_GRAPHQL)
			const result = await orchestrator.generate()

			expect(result).toBeDefined()
			expect(result.code).toBeDefined()
			expect(result.code).toContain('@ObjectType()')
			expect(result.code).toContain('class User')
			expect(result.code).toContain('class Post')
			expect(result.code).toContain('@Field(')
			expect(result.code).toContain('@InputType()')
			expect(result.code).toContain('FilterInput')
			expect(result.code).toContain('SortInput')
			expect(result.code).toContain('Connection')
			expect(result.code).toContain('import { GraphQLJSON }')
			expect(result.code).toContain('type-graphql')

			expect(result.stats.objectTypes).toBeGreaterThanOrEqual(4)
			expect(result.stats.inputTypes).toBeGreaterThan(10)
		})
	})

	describe('Advanced Field Types and Relationships', () => {
		it('should handle all Prisma field types correctly', async () => {
			const context: BaseGeneratorContext = TestFixtures.createContext({
				outputFormat: OutputFormat.GRAPHQL,
				generateScalars: true,
				models: [
					TestFixtures.createDataModel('TestModel', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('stringField', 'String'),
						TestFixtures.createField('intField', 'Int'),
						TestFixtures.createField('bigIntField', 'BigInt'),
						TestFixtures.createField('floatField', 'Float'),
						TestFixtures.createField('decimalField', 'Decimal'),
						TestFixtures.createField('booleanField', 'Boolean'),
						TestFixtures.createField('dateTimeField', 'DateTime'),
						TestFixtures.createField('jsonField', 'Json'),
						TestFixtures.createField('bytesField', 'Bytes'),
						TestFixtures.createField('optionalString', 'String', true),
						TestFixtures.createField('arrayField', 'String', false, true),
					]),
				],
			})

			const orchestrator = new GeneratorOrchestrator(context, OutputFormat.GRAPHQL)
			const result = await orchestrator.generate()

			const generatedSchema = result.sdl
			expect(generatedSchema).toContain('stringField: String!')
			expect(generatedSchema).toContain('intField: Int!')
			expect(generatedSchema).toContain('floatField: Float!')
			expect(generatedSchema).toContain('booleanField: Boolean!')
			expect(generatedSchema).toContain('dateTimeField: DateTime!')
			expect(generatedSchema).toContain('jsonField: JSON!')
			expect(generatedSchema).toContain('optionalString: String')
			expect(generatedSchema).toContain('arrayField: [String!]!')
			expect(generatedSchema).toContain('scalar DateTime')
			expect(generatedSchema).toContain('scalar JSON')
			expect(generatedSchema).toContain('scalar Decimal')
		})

		it('should handle deep nested relationships', async () => {
			const context: BaseGeneratorContext = TestFixtures.createContext({
				outputFormat: OutputFormat.GRAPHQL,
				includeRelations: true,
				connectionTypes: true,
				models: [
					TestFixtures.createDataModel('Organization', [TestFixtures.createField('id', 'String'), TestFixtures.createField('name', 'String')]),
					TestFixtures.createDataModel('Department', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('name', 'String'),
						TestFixtures.createRelationField('organization', 'Organization'),
					]),
					TestFixtures.createDataModel('Team', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('name', 'String'),
						TestFixtures.createRelationField('department', 'Department'),
					]),
					TestFixtures.createDataModel('Member', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('name', 'String'),
						TestFixtures.createRelationField('team', 'Team'),
					]),
					TestFixtures.createDataModel('Project', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('name', 'String'),
						TestFixtures.createRelationField('team', 'Team'),
					]),
					TestFixtures.createDataModel('Task', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('title', 'String'),
						TestFixtures.createField('completed', 'Boolean'),
						TestFixtures.createRelationField('member', 'Member'),
						TestFixtures.createRelationField('project', 'Project', true),
					]),
				],
			})

			const orchestrator = new GeneratorOrchestrator(context, OutputFormat.GRAPHQL)
			const result = await orchestrator.generate()

			const generatedSchema = result.sdl
			expect(generatedSchema).toContain('type Organization')
			expect(generatedSchema).toContain('type Department')
			expect(generatedSchema).toContain('type Team')
			expect(generatedSchema).toContain('type Member')
			expect(generatedSchema).toContain('type Project')
			expect(generatedSchema).toContain('type Task')
		})
	})

	describe('Error Handling and Edge Cases', () => {
		it('should handle empty model gracefully', async () => {
			const context: BaseGeneratorContext = TestFixtures.createContext({
				outputFormat: OutputFormat.GRAPHQL,
				models: [],
				enums: [],
			})

			const orchestrator = new GeneratorOrchestrator(context, OutputFormat.GRAPHQL)
			const result = await orchestrator.generate()

			expect(result).toBeDefined()
			expect(result.sdl).toBeDefined()
		})

		it('should handle model with minimal fields', async () => {
			const context: BaseGeneratorContext = TestFixtures.createContext({
				outputFormat: OutputFormat.GRAPHQL,
				models: [TestFixtures.createDataModel('MinimalModel', [TestFixtures.createField('id', 'String')])],
			})

			const orchestrator = new GeneratorOrchestrator(context, OutputFormat.GRAPHQL)
			const result = await orchestrator.generate()

			expect(result).toBeDefined()
			expect(result.sdl).toBeDefined()
			expect(result.sdl).toContain('type MinimalModel')
			expect(result.sdl).toContain('id: String!')
		})
	})

	describe('Generation Statistics Validation', () => {
		it('should provide accurate generation statistics', async () => {
			const context: BaseGeneratorContext = TestFixtures.createContext({
				outputFormat: OutputFormat.GRAPHQL,
				generateEnums: true,
				generateScalars: true,
				generateFilters: true,
				generateSorts: true,
				connectionTypes: true,
				models: [
					TestFixtures.createDataModel('User', [TestFixtures.createField('id', 'String'), TestFixtures.createField('name', 'String')]),
					TestFixtures.createDataModel('Post', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('title', 'String'),
						TestFixtures.createRelationField('author', 'User'),
					]),
				],
				enums: [TestFixtures.createEnum('Status', ['ACTIVE', 'INACTIVE'])],
			})

			const orchestrator = new GeneratorOrchestrator(context, OutputFormat.GRAPHQL)
			const result = await orchestrator.generate()

			expect(result.stats).toBeDefined()
			expect(result.stats.objectTypes).toBe(2)
			expect(result.stats.enumTypes).toBeGreaterThanOrEqual(1)
			expect(result.stats.scalarTypes).toBeGreaterThan(0)
			expect(result.stats.inputTypes).toBeGreaterThan(0)
			expect(result.stats.generationTimeMs).toBeGreaterThan(0)

			const totalTypes = result.stats.objectTypes + result.stats.enumTypes + result.stats.scalarTypes + result.stats.inputTypes
			expect(totalTypes).toBeGreaterThan(0)
		})
	})

	describe('Custom Configurations', () => {
		it('should respect feature flags correctly', async () => {
			const baseModels = [
				TestFixtures.createDataModel('User', [
					TestFixtures.createField('id', 'String'),
					TestFixtures.createField('name', 'String'),
					TestFixtures.createField('email', 'String'),
				]),
			]

			const contextWithFiltersOnly: BaseGeneratorContext = TestFixtures.createContext({
				outputFormat: OutputFormat.GRAPHQL,
				generateEnums: false,
				generateScalars: false,
				generateFilters: true,
				generateSorts: false,
				connectionTypes: false,
				models: baseModels,
			})

			const orchestrator = new GeneratorOrchestrator(contextWithFiltersOnly, OutputFormat.GRAPHQL)
			const result = await orchestrator.generate()

			expect(result.sdl).toContain('UserFilterInput')
			expect(result.sdl).not.toContain('UserSortInput')
			expect(result.sdl).not.toContain('UserConnection')
			expect(result.stats.enumTypes).toBe(0)
		})

		it('should handle relations inclusion flag', async () => {
			const context: BaseGeneratorContext = TestFixtures.createContext({
				outputFormat: OutputFormat.GRAPHQL,
				includeRelations: false,
				models: [
					TestFixtures.createDataModel('User', [TestFixtures.createField('id', 'String'), TestFixtures.createField('name', 'String')]),
					TestFixtures.createDataModel('Post', [
						TestFixtures.createField('id', 'String'),
						TestFixtures.createField('title', 'String'),
						TestFixtures.createRelationField('author', 'User'),
					]),
				],
			})

			const orchestrator = new GeneratorOrchestrator(context, OutputFormat.GRAPHQL)
			const result = await orchestrator.generate()

			const userType = result.sdl?.match(/type User \{[^}]+\}/)?.[0]
			const postType = result.sdl?.match(/type Post \{[^}]+\}/)?.[0]

			expect(userType).toBeDefined()
			expect(postType).toBeDefined()
		})
	})
})
