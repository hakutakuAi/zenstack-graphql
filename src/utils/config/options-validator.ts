import { z } from 'zod'
import { ErrorCategory, PluginError } from '@utils/error'
import { VALID_SCALAR_VALUES, scalarTypesSchema } from './constants'

export type FieldNaming = 'camelCase' | 'snake_case' | 'preserve'
export type TypeNaming = 'PascalCase' | 'camelCase' | 'preserve'

const optionDefinitions = {
	output: {
		schema: z.string().min(1, 'Output path cannot be empty'),
		default: './schema.graphql',
	},
	scalarTypes: {
		schema: scalarTypesSchema,
		default: {
			DateTime: 'DateTime',
			Json: 'JSON',
			Decimal: 'Decimal',
			Bytes: 'String',
		},
	},
	connectionTypes: {
		schema: z.boolean(),
		default: true,
	},
	generateEnums: {
		schema: z.boolean(),
		default: true,
	},
	generateScalars: {
		schema: z.boolean(),
		default: true,
	},
	generateFilters: {
		schema: z.boolean(),
		default: true,
	},
	generateSorts: {
		schema: z.boolean(),
		default: true,
	},
	fieldNaming: {
		schema: z.enum(['camelCase', 'snake_case', 'preserve']),
		default: 'camelCase' as FieldNaming,
	},
	typeNaming: {
		schema: z.enum(['PascalCase', 'camelCase', 'preserve']),
		default: 'PascalCase' as TypeNaming,
	},
	includeRelations: {
		schema: z.boolean(),
		default: true,
	},
	schemaPath: {
		schema: z.string().optional(),
		default: undefined,
	},
}

type OptionDefinitions = typeof optionDefinitions
type OptionKey = keyof OptionDefinitions

export const DEFAULT_OPTIONS = Object.fromEntries(Object.entries(optionDefinitions).map(([key, def]) => [key, def.default])) as { [K in OptionKey]: OptionDefinitions[K]['default'] }

export type PluginOptions = {
	[K in OptionKey]?: z.input<OptionDefinitions[K]['schema']>
}

export type NormalizedOptions = {
	[K in OptionKey as OptionDefinitions[K]['default'] extends undefined ? never : K]: z.output<OptionDefinitions[K]['schema']> extends infer T ? (T extends undefined ? never : NonNullable<T>) : never
}

const optionsSchema = z.object(
	Object.fromEntries(Object.entries(optionDefinitions).map(([key, def]) => [key, def.schema.optional()])) as { [K in OptionKey]: z.ZodOptional<OptionDefinitions[K]['schema']> }
)

export function validateOptions(options: PluginOptions = {}): NormalizedOptions {
	try {
		const validatedOptions = optionsSchema.parse(options)

		const normalized = Object.fromEntries(
			Object.entries(optionDefinitions)
				.filter(([_, def]) => def.default !== undefined)
				.map(([key, def]) => {
					const value = validatedOptions[key as OptionKey]
					if (value !== undefined) {
						if (key === 'scalarTypes' && typeof def.default === 'object' && def.default !== null && typeof value === 'object' && value !== null) {
							return [key, { ...def.default, ...value }]
						}
						return [key, value]
					}
					return [key, def.default]
				})
		) as NormalizedOptions

		return normalized
	} catch (error) {
		if (error instanceof z.ZodError) {
			const errorMessages = error.issues.map((issue) => `${issue.path.join('.') || 'root'}: ${issue.message}`).join('\n')

			const suggestions = error.issues.map((issue) => {
				const path = issue.path.join('.') || 'option'

				if (issue.code === 'invalid_type') {
					return `Ensure ${path} is of type ${issue.expected}`
				}
				if (issue.code === 'invalid_enum_value' && 'options' in issue) {
					return `${path} must be one of: ${issue.options.join(', ')}`
				}
				if (issue.code === 'custom' && path.includes('scalarTypes')) {
					return `Use valid GraphQL scalar types: Standard types are ${VALID_SCALAR_VALUES.join(', ')}. Custom types must start with an uppercase letter or underscore and contain only letters, numbers, and underscores.`
				}
				return `Check the ${path} option value`
			})

			const formattedOptions = JSON.stringify(options, null, 2)
			const formattedErrors = JSON.stringify(error.issues, null, 2)

			throw new PluginError(
				`Invalid plugin options:\n${errorMessages}`,
				ErrorCategory.VALIDATION,
				{
					originalOptions: formattedOptions,
					validationErrors: formattedErrors,
					errorCount: error.issues.length,
				},
				suggestions.length ? suggestions : ['Review the plugin documentation for valid option values']
			)
		}

		const errorMessage = error instanceof Error ? error.message : String(error)
		throw new PluginError(`Error during options validation: ${errorMessage}`, ErrorCategory.VALIDATION, { originalError: error })
	}
}

export default validateOptions
