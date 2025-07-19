export * from '@generators'
export * from '@utils'

import type { PluginOptions as SdkPluginOptions } from '@zenstackhq/sdk'
import { DataModel, Enum, isDataModel, isEnum, type Model } from '@zenstackhq/sdk/ast'
import { ok, err } from 'neverthrow'

import { CoreGenerator } from '@generators'
import { ErrorCategory, PluginErrorData, createError, PluginResult } from '@utils/error'
import { validateOptions, PluginOptions } from '@utils/config'
import { FileWriter } from '@utils/io/file-writer'

export const name = 'ZenStack GraphQL'
export const description = 'Generates GraphQL schemas'

export interface PluginMetadata {
	stats: {
		objectTypes: number
		enumTypes: number
		scalarTypes: number
		relationFields: number
		connectionTypes: number
	}
	outputPath: string
}

export default async function run(model: Model, options: SdkPluginOptions): Promise<PluginResult<{ metadata: PluginMetadata }>> {
	if (!model) {
		return createError(
			'Model is required',
			ErrorCategory.VALIDATION,
			{
				model: model ? 'Provided' : 'Missing',
			},
			['Ensure that the model is correctly passed to the plugin.', 'Check your ZModel schema for completeness.']
		)
	}

	const optionsResult = validateOptions(options as PluginOptions)
	if (optionsResult.isErr()) {
		return err(optionsResult.error)
	}
	const normalizedOptions = optionsResult.value

	try {
		const models = model.declarations.filter((x) => isDataModel(x) && !x.isAbstract) as DataModel[]
		const enums = model.declarations.filter((x) => isEnum(x)) as Enum[]

		const coreGenerator = new CoreGenerator({
			options: normalizedOptions,
			models,
			enums,
		})

		const result = coreGenerator.generate()
		const fileWriter = FileWriter.create()

		const writeResult = await fileWriter.writeSchema(result.sdl, normalizedOptions.output)
		if (writeResult.isErr()) {
			return err(writeResult.error)
		}

		return ok({
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
		})
	} catch (error) {
		if ((error as any).isPluginError) {
			return err(error as PluginErrorData)
		}

		if (error instanceof Error && error.message.includes('validation')) {
			return createError('Invalid plugin options', ErrorCategory.VALIDATION, { originalError: error })
		}

		return createError('GraphQL schema generation failed', ErrorCategory.GENERATION, { originalError: error }, [
			'Check your ZModel schema for errors',
			'Verify plugin configuration options',
			'Ensure all required models and fields are properly defined',
		])
	}
}
