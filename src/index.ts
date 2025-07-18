export * from '@generators'
export * from '@utils'

import type { PluginOptions as SdkPluginOptions } from '@zenstackhq/sdk'
import type { DMMF } from '@zenstackhq/sdk/prisma'
import type { Model } from '@zenstackhq/sdk/ast'
import { SchemaComposer } from 'graphql-compose'

import { CoreGenerator } from '@generators'
import { ErrorHandler, GenerationError, ValidationError, validateOptions, PluginOptions, AttributeProcessor, TypeMapper } from '@utils'

import fileWriter from '@utils/file-writer'

export const name = 'ZenStack GraphQL'
export const description = 'Generates GraphQL schemas'

export default async function run(model: Model, options: SdkPluginOptions, dmmf: DMMF.Document) {
	const errorHandler = ErrorHandler.getInstance()

	try {
		const normalizedOptions = validateOptions(options as PluginOptions, errorHandler)
		const schemaComposer = new SchemaComposer()
		const attributeProcessor = new AttributeProcessor()
		const typeMapper = TypeMapper.createFromDMMF(dmmf)

		const coreGenerator = new CoreGenerator({
			model,
			options: normalizedOptions,
			dmmf,
			errorHandler,
			attributeProcessor,
			typeMapper,
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
		if (error instanceof ValidationError) {
			throw errorHandler.createValidationError('Invalid plugin options', { originalError: error })
		}

		if (error instanceof GenerationError) {
			throw error
		}

		throw errorHandler.createGenerationError('GraphQL schema generation failed', { originalError: error }, [
			'Check your ZModel schema for errors',
			'Verify plugin configuration options',
			'Ensure all required models and fields are properly defined',
		])
	}
}
