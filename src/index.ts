export * from '@generators'
export * from '@utils'

import type { PluginOptions as SdkPluginOptions } from '@zenstackhq/sdk'
import type { DMMF } from '@zenstackhq/sdk/prisma'
import type { Model } from '@zenstackhq/sdk/ast'
import { SchemaComposer } from 'graphql-compose'

import { CoreGenerator } from '@generators'
import { ErrorHandler, PluginError, ErrorCategory, ErrorSeverity, validateOptions, PluginOptions, AttributeProcessor, TypeMapper } from '@utils'
import { TypeFormatter } from '@utils/schema'

import fileWriter from './utils/io/file-writer'

export const name = 'ZenStack GraphQL'
export const description = 'Generates GraphQL schemas'

export default async function run(model: Model, options: SdkPluginOptions, dmmf: DMMF.Document) {
	const errorHandler = ErrorHandler.getInstance()

	try {
		const normalizedOptions = validateOptions(options as PluginOptions, errorHandler)
		const schemaComposer = new SchemaComposer()
		const attributeProcessor = new AttributeProcessor()
		const typeMapper = TypeMapper.createFromDMMF(dmmf)
		const typeFormatter = TypeFormatter.fromOptions(normalizedOptions.typeNaming, normalizedOptions.fieldNaming)

		const coreGenerator = new CoreGenerator({
			model,
			options: normalizedOptions,
			dmmf,
			errorHandler,
			attributeProcessor,
			typeMapper,
			typeFormatter,
			schemaComposer,
		})

		const result = coreGenerator.generateSchema()

		await fileWriter.writeSchema(result.sdl, normalizedOptions.output)

		return {
			warnings: result.stats.warnings,
			metadata: {
				stats: {
					objectTypes: result.stats.objectTypes,
					enumTypes: result.stats.enumTypes,
					scalarTypes: result.stats.scalarTypes,
					relationFields: result.stats.relationFields,
					connectionTypes: result.stats.connectionTypes,
				},
				outputPath: normalizedOptions.output,
			},
		}
	} catch (error) {
		if (error instanceof PluginError) {
			throw error
		}

		if (error instanceof Error && error.message.includes('validation')) {
			throw errorHandler.createError('Invalid plugin options', ErrorCategory.VALIDATION, ErrorSeverity.ERROR, { originalError: error })
		}

		throw errorHandler.createError('GraphQL schema generation failed', ErrorCategory.GENERATION, ErrorSeverity.ERROR, { originalError: error }, [
			'Check your ZModel schema for errors',
			'Verify plugin configuration options',
			'Ensure all required models and fields are properly defined',
		])
	}
}
