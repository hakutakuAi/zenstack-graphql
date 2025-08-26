import { describe, it, expect, beforeEach } from 'bun:test'
import run from '@/index'
import { SchemaBuilder } from '../helpers'
import { OutputFormat } from '@utils/constants'
import { getTestOutputPath } from '../test-setup'

describe('Complete Generation Integration', () => {
	describe('GraphQL Schema Generation', () => {
		it('should generate complete GraphQL schema from simple model', async () => {
			const schema = SchemaBuilder.createSimpleUser().build()
			const options = {
				output: getTestOutputPath('test-output.graphql'),
				outputFormat: OutputFormat.GRAPHQL,
				schemaPath: getTestOutputPath('test.zmodel'),
				provider: 'postgresql',
			}

			const result = await run(schema, options)

			expect(result).toHaveProperty('metadata')
			expect(result.metadata).toHaveProperty('outputPath')
			expect(result.metadata).toHaveProperty('stats')
		})

		it('should generate complete GraphQL schema from simple model without DateTime', async () => {
			const schema = SchemaBuilder.create()
				.addModel('User')
				.addField('id', 'String')
				.addField('name', 'String')
				.addField('email', 'String')
				.addField('age', 'Int', true)
				.addField('active', 'Boolean')
				.finish()
				.build()

			const options = {
				output: getTestOutputPath('simple-schema.graphql'),
				outputFormat: OutputFormat.GRAPHQL,
				schemaPath: getTestOutputPath('simple.zmodel'),
				provider: 'postgresql',
				generateFilters: false,
				generateSorts: false,
				generateScalars: false,
			}

			const result = await run(schema, options)

			expect(result).toHaveProperty('metadata')
			expect(result.metadata.stats).toHaveProperty('objectTypes')
			expect(result.metadata.stats.objectTypes).toBeGreaterThan(0)
		})
	})

	describe('TypeScript Generation', () => {
		it('should generate TypeScript types from simple model', async () => {
			const schema = SchemaBuilder.createSimpleUser().build()
			const options = {
				output: getTestOutputPath('types.ts'),
				outputFormat: OutputFormat.TYPE_GRAPHQL,
				schemaPath: getTestOutputPath('test.zmodel'),
				provider: 'postgresql',
			}

			const result = await run(schema, options)

			expect(result).toHaveProperty('metadata')
			expect(result.metadata).toHaveProperty('outputPath')
		})
	})

	describe('Feature Flags', () => {
		it('should respect generateScalars flag', async () => {
			const schema = SchemaBuilder.createSimpleUser().build()
			const options = {
				output: getTestOutputPath('test.graphql'),
				outputFormat: OutputFormat.GRAPHQL,
				generateScalars: false,
				schemaPath: getTestOutputPath('test.zmodel'),
				provider: 'postgresql',
			}

			const result = await run(schema, options)

			expect(result).toHaveProperty('metadata')
			expect(result.metadata.stats).toBeDefined()
		})

		it('should respect generateEnums flag', async () => {
			const schema = SchemaBuilder.createBlogSchema().build()
			const options = {
				output: getTestOutputPath('test-enum.graphql'),
				outputFormat: OutputFormat.GRAPHQL,
				generateEnums: false,
				schemaPath: getTestOutputPath('test.zmodel'),
				provider: 'postgresql',
			}

			const result = await run(schema, options)

			expect(result).toHaveProperty('metadata')
			expect(result.metadata.stats).toBeDefined()
		})
	})

	describe('Error Scenarios', () => {
		it('should handle invalid output path gracefully', async () => {
			const schema = SchemaBuilder.createSimpleUser().build()
			const options = {
				output: '/invalid/path/schema.graphql',
				outputFormat: OutputFormat.GRAPHQL,
				schemaPath: getTestOutputPath('test.zmodel'),
				provider: 'postgresql',
			}

			expect(run(schema, options)).rejects.toThrow()
		})

		it('should handle malformed schema gracefully', async () => {
			const schema = null as any
			const options = {
				output: getTestOutputPath('test-malformed.graphql'),
				outputFormat: OutputFormat.GRAPHQL,
				schemaPath: getTestOutputPath('test.zmodel'),
				provider: 'postgresql',
			}

			expect(run(schema, options)).rejects.toThrow()
		})
	})
})
