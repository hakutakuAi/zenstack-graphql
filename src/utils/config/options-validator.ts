import { z } from 'zod'
import { Result, ok, err } from 'neverthrow'
import { ErrorCategory, PluginErrorData, createError } from '@utils/error'
import { VALID_SCALAR_VALUES, scalarTypesSchema } from './constants'

export type FieldNaming = 'camelCase' | 'snake_case' | 'preserve'
export type TypeNaming = 'PascalCase' | 'camelCase' | 'preserve'

const DEFAULT_OPTIONS = {
	output: './schema.graphql',
	scalarTypes: {
		DateTime: 'DateTime',
		Json: 'JSON',
		Decimal: 'Decimal',
		Bytes: 'String',
	},
	connectionTypes: true,
	generateEnums: true,
	generateScalars: true,
	fieldNaming: 'camelCase' as FieldNaming,
	typeNaming: 'PascalCase' as TypeNaming,
	includeRelations: true,
}

export type PluginOptions = {
	output?: string
	scalarTypes?: Record<string, string>
	connectionTypes?: boolean
	generateEnums?: boolean
	generateScalars?: boolean
	fieldNaming?: FieldNaming
	typeNaming?: TypeNaming
	includeRelations?: boolean
}

export type NormalizedOptions = {
	output: string
	scalarTypes: Record<string, string>
	connectionTypes: boolean
	generateEnums: boolean
	generateScalars: boolean
	fieldNaming: FieldNaming
	typeNaming: TypeNaming
	includeRelations: boolean
}

const fieldNamingSchema = z.enum(['camelCase', 'snake_case', 'preserve'])
const typeNamingSchema = z.enum(['PascalCase', 'camelCase', 'preserve'])

const optionsSchema = z.object({
	output: z.string().min(1, 'Output path cannot be empty').optional(),
	scalarTypes: scalarTypesSchema.optional(),
	connectionTypes: z.boolean().optional(),
	generateEnums: z.boolean().optional(),
	generateScalars: z.boolean().optional(),
	fieldNaming: fieldNamingSchema.optional(),
	typeNaming: typeNamingSchema.optional(),
	includeRelations: z.boolean().optional(),
	schemaPath: z.string().optional(),
})

export function validateOptions(options: PluginOptions = {}): Result<NormalizedOptions, PluginErrorData> {
	try {
		const validatedOptions = optionsSchema.parse(options)

		return ok({
			output: validatedOptions.output ?? DEFAULT_OPTIONS.output,
			scalarTypes: {
				...DEFAULT_OPTIONS.scalarTypes,
				...validatedOptions.scalarTypes,
			},
			connectionTypes: validatedOptions.connectionTypes ?? DEFAULT_OPTIONS.connectionTypes,
			generateEnums: validatedOptions.generateEnums ?? DEFAULT_OPTIONS.generateEnums,
			generateScalars: validatedOptions.generateScalars ?? DEFAULT_OPTIONS.generateScalars,
			fieldNaming: validatedOptions.fieldNaming ?? DEFAULT_OPTIONS.fieldNaming,
			typeNaming: validatedOptions.typeNaming ?? DEFAULT_OPTIONS.typeNaming,
			includeRelations: validatedOptions.includeRelations ?? DEFAULT_OPTIONS.includeRelations,
		})
	} catch (error) {
		if (error instanceof z.ZodError) {
			const errorMessages = error.issues
				.map((issue) => {
					const path = issue.path.length > 0 ? issue.path.join('.') : 'root'
					return `${path}: ${issue.message}`
				})
				.join('\n')

			const suggestions = generateSuggestions(error.issues)

			return createError(
				`Invalid plugin options:\n${errorMessages}`,
				ErrorCategory.VALIDATION,
				{
					originalOptions: options,
					validationErrors: error.issues,
					errorCount: error.issues.length,
				},
				suggestions
			)
		}

		return createError(`Error during options validation: ${error instanceof Error ? error.message : String(error)}`, ErrorCategory.VALIDATION, { originalError: error })
	}
}

function generateSuggestions(issues: z.ZodIssue[]): string[] {
	const suggestions = issues.map((issue) => {
		const path = issue.path.join('.') || 'option'

		switch (issue.code) {
			case 'invalid_type':
				return `Ensure ${path} is of type ${issue.expected}`
			case 'invalid_enum_value':
				if ('options' in issue) {
					return `${path} must be one of: ${issue.options.join(', ')}`
				}
				break
			case 'custom':
				if (path.includes('scalarTypes')) {
					return `Use valid GraphQL scalar types: ${VALID_SCALAR_VALUES.join(', ')}`
				}
				break
		}

		return `Check the ${path} option value`
	})

	return suggestions.length ? suggestions : ['Review the plugin documentation for valid option values']
}

export default validateOptions
