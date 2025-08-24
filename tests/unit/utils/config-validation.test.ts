import { describe, test, expect } from 'bun:test'
import { validateOptions, DEFAULT_OPTIONS, PluginOptions } from '@utils/config'
import { PluginError, ErrorCategory } from '@utils/error'
import { OutputFormat } from '@utils/constants'
import { getTestOutputPath } from '../../test-setup'

describe('Config Validation', () => {
	describe('Default Options', () => {
		test('should have correct default values', () => {
			expect(DEFAULT_OPTIONS.output).toBe('./schema.graphql')
			expect(DEFAULT_OPTIONS.outputFormat).toBe(OutputFormat.GRAPHQL)
			expect(DEFAULT_OPTIONS.generateScalars).toBe(true)
			expect(DEFAULT_OPTIONS.generateEnums).toBe(true)
			expect(DEFAULT_OPTIONS.generateFilters).toBe(true)
			expect(DEFAULT_OPTIONS.generateSorts).toBe(true)
			expect(DEFAULT_OPTIONS.connectionTypes).toBe(true)
			expect(DEFAULT_OPTIONS.includeRelations).toBe(true)
			expect(DEFAULT_OPTIONS.fieldNaming).toBe('camelCase')
			expect(DEFAULT_OPTIONS.typeNaming).toBe('PascalCase')
		})

		test('should have correct scalar type defaults', () => {
			expect(DEFAULT_OPTIONS.scalarTypes.DateTime).toBe('DateTime')
			expect(DEFAULT_OPTIONS.scalarTypes.Json).toBe('JSON')
			expect(DEFAULT_OPTIONS.scalarTypes.Decimal).toBe('Decimal')
			expect(DEFAULT_OPTIONS.scalarTypes.Bytes).toBe('String')
		})
	})

	describe('Valid Options', () => {
		test('should validate minimal valid options', () => {
			const options: PluginOptions = {}
			const normalized = validateOptions(options)

			expect(normalized).toBeDefined()
			expect(normalized.output).toBe(DEFAULT_OPTIONS.output)
			expect(normalized.outputFormat).toBe(DEFAULT_OPTIONS.outputFormat)
		})

		test('should validate custom output path', () => {
			const options: PluginOptions = {
				output: getTestOutputPath('custom/schema.graphql'),
			}
			const normalized = validateOptions(options)

			expect(normalized.output).toBe(getTestOutputPath('custom/schema.graphql'))
		})

		test('should validate GraphQL output format', () => {
			const options: PluginOptions = {
				outputFormat: OutputFormat.GRAPHQL,
			}
			const normalized = validateOptions(options)

			expect(normalized.outputFormat).toBe(OutputFormat.GRAPHQL)
		})

		test('should validate TypeScript output format', () => {
			const options: PluginOptions = {
				outputFormat: OutputFormat.TYPE_GRAPHQL,
			}
			const normalized = validateOptions(options)

			expect(normalized.outputFormat).toBe(OutputFormat.TYPE_GRAPHQL)
		})

		test('should validate boolean feature flags', () => {
			const options: PluginOptions = {
				generateScalars: false,
				generateEnums: false,
				generateFilters: false,
				generateSorts: false,
				connectionTypes: false,
				includeRelations: false,
			}
			const normalized = validateOptions(options)

			expect(normalized.generateScalars).toBe(false)
			expect(normalized.generateEnums).toBe(false)
			expect(normalized.generateFilters).toBe(false)
			expect(normalized.generateSorts).toBe(false)
			expect(normalized.connectionTypes).toBe(false)
			expect(normalized.includeRelations).toBe(false)
		})

		test('should validate field naming options', () => {
			const testCases = ['camelCase', 'snake_case', 'preserve'] as const

			for (const naming of testCases) {
				const options: PluginOptions = { fieldNaming: naming }
				const normalized = validateOptions(options)
				expect(normalized.fieldNaming).toBe(naming)
			}
		})

		test('should validate type naming options', () => {
			const testCases = ['PascalCase', 'camelCase', 'preserve'] as const

			for (const naming of testCases) {
				const options: PluginOptions = { typeNaming: naming }
				const normalized = validateOptions(options)
				expect(normalized.typeNaming).toBe(naming)
			}
		})

		test('should validate custom scalar types', () => {
			const options: PluginOptions = {
				scalarTypes: {
					DateTime: 'CustomDateTime',
					Json: 'CustomJSON',
					CustomType: 'String',
				},
			}
			const normalized = validateOptions(options)

			expect(normalized.scalarTypes.DateTime).toBe('CustomDateTime')
			expect(normalized.scalarTypes.Json).toBe('CustomJSON')
			expect(normalized.scalarTypes.CustomType).toBe('String')
		})

		test('should merge scalar types with defaults', () => {
			const options: PluginOptions = {
				scalarTypes: {
					DateTime: 'CustomDateTime',
				},
			}
			const normalized = validateOptions(options)

			expect(normalized.scalarTypes.DateTime).toBe('CustomDateTime')
			expect(normalized.scalarTypes.Json).toBe('JSON')
			expect(normalized.scalarTypes.Decimal).toBe('Decimal')
		})
	})

	describe('Invalid Options', () => {
		test('should throw PluginError for empty output path', () => {
			const options: PluginOptions = {
				output: '',
			}

			expect(() => validateOptions(options)).toThrow(PluginError)
		})

		test('should throw PluginError for invalid output format', () => {
			const options: PluginOptions = {
				outputFormat: 'invalid' as any,
			}

			expect(() => validateOptions(options)).toThrow(PluginError)
		})

		test('should throw PluginError for invalid field naming', () => {
			const options: PluginOptions = {
				fieldNaming: 'invalid' as any,
			}

			expect(() => validateOptions(options)).toThrow(PluginError)
		})

		test('should throw PluginError for invalid type naming', () => {
			const options: PluginOptions = {
				typeNaming: 'invalid' as any,
			}

			expect(() => validateOptions(options)).toThrow(PluginError)
		})

		test('should throw PluginError for invalid boolean values', () => {
			const options: PluginOptions = {
				generateScalars: 'yes' as any,
			}

			expect(() => validateOptions(options)).toThrow(PluginError)
		})

		test('should throw PluginError for invalid scalar type name', () => {
			const options: PluginOptions = {
				scalarTypes: {
					DateTime: 'invalid-name-with-hyphens',
				},
			}

			expect(() => validateOptions(options)).toThrow(PluginError)
		})
	})

	describe('Error Handling', () => {
		test('should provide detailed error information', () => {
			const options: PluginOptions = {
				output: '',
				outputFormat: 'invalid' as any,
			}

			try {
				validateOptions(options)
			} catch (error) {
				expect(error).toBeInstanceOf(PluginError)
				const pluginError = error as PluginError
				expect(pluginError.category).toBe(ErrorCategory.VALIDATION)
				expect(pluginError.message).toContain('Invalid plugin options')
				expect(pluginError.context).toBeDefined()
				expect(pluginError.suggestions).toBeDefined()
			}
		})

		test('should include original options in error context', () => {
			const options: PluginOptions = {
				output: '',
			}

			try {
				validateOptions(options)
			} catch (error) {
				const pluginError = error as PluginError
				expect(pluginError.context!.originalOptions).toBeDefined()
				expect(pluginError.context!.originalOptions).toContain('"output": ""')
			}
		})

		test('should provide helpful suggestions for common errors', () => {
			const options: PluginOptions = {
				fieldNaming: 'kebab-case' as any,
			}

			try {
				validateOptions(options)
			} catch (error) {
				const pluginError = error as PluginError
				expect(pluginError.suggestions).toBeDefined()
				expect(pluginError.suggestions!.length).toBeGreaterThan(0)
				expect(pluginError.suggestions!.some((s) => s.includes('camelCase, snake_case, preserve'))).toBe(true)
			}
		})

		test('should handle scalar type validation errors', () => {
			const options: PluginOptions = {
				scalarTypes: {
					DateTime: '123Invalid',
				},
			}

			try {
				validateOptions(options)
			} catch (error) {
				const pluginError = error as PluginError
				expect(pluginError.message).toContain('Invalid scalar type mapping')
				expect(pluginError.suggestions!.some((s) => s.includes('GraphQL scalar types'))).toBe(true)
			}
		})
	})

	describe('Edge Cases', () => {
		test('should handle null options gracefully', () => {
			expect(() => validateOptions(null as any)).toThrow(PluginError)
		})

		test('should handle undefined options', () => {
			const normalized = validateOptions(undefined)
			expect(normalized).toBeDefined()
			expect(normalized.output).toBe(DEFAULT_OPTIONS.output)
		})

		test('should handle empty object', () => {
			const normalized = validateOptions({})
			expect(normalized).toBeDefined()
			expect(normalized.output).toBe(DEFAULT_OPTIONS.output)
		})

		test('should handle partial scalar type overrides', () => {
			const options: PluginOptions = {
				scalarTypes: {
					DateTime: 'Timestamp',
				},
			}
			const normalized = validateOptions(options)

			expect(normalized.scalarTypes.DateTime).toBe('Timestamp')
			expect(normalized.scalarTypes.Json).toBe('JSON')
			expect(normalized.scalarTypes.Decimal).toBe('Decimal')
			expect(normalized.scalarTypes.Bytes).toBe('String')
		})

		test('should validate standard GraphQL scalars', () => {
			const options: PluginOptions = {
				scalarTypes: {
					CustomField: 'String',
					AnotherField: 'Int',
					ThirdField: 'Float',
					BooleanField: 'Boolean',
					IDField: 'ID',
				},
			}

			expect(() => validateOptions(options)).not.toThrow()
		})

		test('should validate custom GraphQL scalar names', () => {
			const options: PluginOptions = {
				scalarTypes: {
					CustomField: 'MyCustomScalar',
					AnotherField: '_UnderscoreScalar',
					ThirdField: 'Scalar123',
				},
			}

			expect(() => validateOptions(options)).not.toThrow()
		})
	})
})
