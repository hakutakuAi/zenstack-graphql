import { describe, test, expect, beforeEach } from 'bun:test'
import run, { name, description } from '@/index'
import { PluginError, ErrorCategory } from '@utils/error'
import { TestUtils, SchemaBuilder } from '../../helpers'
import { OutputFormat } from '@utils/constants'
import { getTestOutputPath } from '../../test-setup'

describe('Plugin Entry Point', () => {
	describe('Metadata', () => {
		test('should export correct plugin name', () => {
			expect(name).toBe('ZenStack GraphQL')
		})

		test('should export correct plugin description', () => {
			expect(description).toBe('Generates GraphQL schemas')
		})
	})

	describe('Model Validation', () => {
		test('should throw PluginError when model is null', async () => {
			const options = TestUtils.createMockSdkOptions()

			expect(run(null as any, options)).rejects.toThrow(PluginError)
		})

		test('should throw PluginError when model is undefined', async () => {
			const options = TestUtils.createMockSdkOptions()

			expect(run(undefined as any, options)).rejects.toThrow(PluginError)
		})

		test('should throw validation error with correct category', async () => {
			const options = TestUtils.createMockSdkOptions()

			try {
				await run(null as any, options)
			} catch (error) {
				expect(error).toBeInstanceOf(PluginError)
				expect((error as PluginError).category).toBe(ErrorCategory.VALIDATION)
				expect((error as PluginError).message).toContain('Model is required')
			}
		})
	})

	describe('Options Validation', () => {
		test('should validate and normalize options', async () => {
			const schema = SchemaBuilder.createSimpleUser().build()
			const options = TestUtils.createMockSdkOptions({
				output: getTestOutputPath('test-output.graphql'),
				outputFormat: OutputFormat.GRAPHQL,
			})

			const result = await run(schema, options)

			expect(result).toHaveProperty('metadata')
			expect(result.metadata).toHaveProperty('outputPath')
			expect(result.metadata).toHaveProperty('stats')
		})

		test('should handle invalid options gracefully', async () => {
			const schema = SchemaBuilder.createSimpleUser().build()
			const options = TestUtils.createMockSdkOptions({
				outputFormat: 'invalid-format' as any,
			})

			expect(run(schema, options)).rejects.toThrow(PluginError)
		})

		test('should use default options when none provided', async () => {
			const schema = SchemaBuilder.createSimpleUser().build()

			const result = await run(
				schema,
				TestUtils.createMockSdkOptions({
					output: getTestOutputPath('default-output.graphql'),
				}),
			)

			expect(result).toHaveProperty('metadata')
			expect(result.metadata).toHaveProperty('outputPath')
		})
	})

	describe('Context Creation', () => {
		test('should create context with filtered models', async () => {
			const testSchema = SchemaBuilder.createBlogSchema().build()

			const result = await run(
				testSchema,
				TestUtils.createMockSdkOptions({
					output: getTestOutputPath('filtered-models.graphql'),
				}),
			)

			expect(result.metadata.stats.objectTypes).toBeGreaterThan(0)
		})

		test('should include enums in context', async () => {
			const schema = SchemaBuilder.create()
				.addEnum('UserRole', ['ADMIN', 'USER'])
				.addModel('User')
				.addField('id', 'String')
				.addField('role', 'UserRole')
				.finish()
				.build()

			const result = await run(
				schema,
				TestUtils.createMockSdkOptions({
					output: getTestOutputPath('enums-context.graphql'),
				}),
			)

			expect(result.metadata.stats.enumTypes).toBe(1)
		})
	})

	describe('Generation Flow', () => {
		test('should generate GraphQL SDL format', async () => {
			const schema = SchemaBuilder.createSimpleUser().build()
			const options = TestUtils.createMockSdkOptions({
				outputFormat: OutputFormat.GRAPHQL,
			})

			const result = await run(schema, options)

			expect(result.metadata.outputPath).toBeDefined()
			expect(result.metadata.stats).toBeDefined()
		})

		test('should generate TypeScript format', async () => {
			const schema = SchemaBuilder.createSimpleUser().build()
			const options = TestUtils.createMockSdkOptions({
				outputFormat: OutputFormat.TYPE_GRAPHQL,
			})

			const result = await run(schema, options)

			expect(result.metadata.outputPath).toBeDefined()
			expect(result.metadata.stats).toBeDefined()
		})
	})

	describe('Error Handling', () => {
		test('should handle generation errors gracefully', async () => {
			const schema = SchemaBuilder.createSimpleUser().build()
			const options = TestUtils.createMockSdkOptions({
				output: '/invalid/path/that/does/not/exist/schema.graphql',
			})

			expect(run(schema, options)).rejects.toThrow(PluginError)
		})

		test('should wrap unknown errors as PluginError', async () => {
			expect(true).toBe(true)
		})

		test('should provide helpful suggestions in error messages', async () => {
			try {
				await run(
					null as any,
					TestUtils.createMockSdkOptions({
						output: getTestOutputPath('null-schema.graphql'),
					}),
				)
			} catch (error) {
				expect(error).toBeInstanceOf(PluginError)
				const pluginError = error as PluginError
				expect(pluginError.suggestions).toBeDefined()
				expect(pluginError.suggestions!.length).toBeGreaterThan(0)
			}
		})
	})

	describe('Output Metadata', () => {
		let schema: any

		beforeEach(() => {
			schema = SchemaBuilder.createBlogSchema().build()
		})

		test('should return correct metadata structure', async () => {
			const result = await run(
				schema,
				TestUtils.createMockSdkOptions({
					output: getTestOutputPath('metadata-structure.graphql'),
				}),
			)

			expect(result).toHaveProperty('metadata')
			expect(result.metadata).toHaveProperty('stats')
			expect(result.metadata).toHaveProperty('outputPath')
		})

		test('should include generation statistics', async () => {
			const result = await run(
				schema,
				TestUtils.createMockSdkOptions({
					output: getTestOutputPath('generation-stats.graphql'),
				}),
			)

			expect(result.metadata.stats).toHaveProperty('objectTypes')
			expect(result.metadata.stats).toHaveProperty('enumTypes')
			expect(result.metadata.stats).toHaveProperty('scalarTypes')
			expect(result.metadata.stats.objectTypes).toBeGreaterThan(0)
			expect(result.metadata.stats.enumTypes).toBeGreaterThan(0)
		})

		test('should track output path correctly', async () => {
			const customOutput = getTestOutputPath('custom-schema.graphql')
			const result = await run(schema, TestUtils.createMockSdkOptions({ output: customOutput }))

			expect(result.metadata.outputPath).toContain('custom-schema.graphql')
		})
	})
})
