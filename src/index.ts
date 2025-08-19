import 'reflect-metadata'

export * from '@generators'

import type { PluginOptions as SdkPluginOptions } from '@zenstackhq/sdk'
import { DataModel, Enum, isDataModel, isEnum, type Model } from '@zenstackhq/sdk/ast'

import { CoreGenerator } from '@generators/core-generator'
import { CoreGenerator as TypeScriptCoreGenerator } from '@generators/outputs/typescript/core-generator'
import { ErrorCategory, PluginError } from '@utils/error'
import { validateOptions, PluginOptions } from '@utils/config'
import { FileWriter } from '@utils/file-writer'
import { OutputFormat } from '@utils/constants'

export const name = 'ZenStack GraphQL'
export const description = 'Generates GraphQL schemas'

export interface PluginMetadata {
	stats: {
		objectTypes: number
		enumTypes: number
		scalarTypes: number
		relationFields: number
		connectionTypes: number
		sortInputTypes: number
		filterInputTypes: number
	}
	outputPath: string
}

export default async function run(model: Model, options: SdkPluginOptions): Promise<{ metadata: PluginMetadata }> {
	if (!model) {
		throw new PluginError(
			'Model is required',
			ErrorCategory.VALIDATION,
			{
				model: model ? 'Provided' : 'Missing',
			},
			['Ensure that the model is correctly passed to the plugin.', 'Check your ZModel schema for completeness.'],
		)
	}

	try {
		const normalizedOptions = validateOptions(options as PluginOptions)
		const models = model.declarations.filter((x) => isDataModel(x) && !x.isAbstract) as DataModel[]
		const enums = model.declarations.filter((x) => isEnum(x)) as Enum[]

		if (normalizedOptions.outputFormat === OutputFormat.TYPE_GRAPHQL) {
			const typeScriptGenerator = new TypeScriptCoreGenerator({
				options: normalizedOptions,
				models,
				enums,
			})

			const result = typeScriptGenerator.generate()
			const outputPath = normalizedOptions.output.replace(/\.graphql$/, '.ts')
			await FileWriter.create().writeTypeGraphQL(result.code, outputPath)

			return {
				metadata: {
					stats: {
						objectTypes: result.stats.objectTypes.length,
						enumTypes: result.stats.enumTypes.length,
						scalarTypes: 0,
						relationFields: 0,
						connectionTypes: 0,
						sortInputTypes: 0,
						filterInputTypes: 0,
					},
					outputPath,
				},
			}
		} else {
			const coreGenerator = new CoreGenerator({
				options: normalizedOptions,
				models,
				enums,
			})

			const result = coreGenerator.generate()
			await FileWriter.create().writeSchema(result.sdl, normalizedOptions.output)

			return {
				metadata: {
					stats: {
						objectTypes: result.stats.objectTypes.length,
						enumTypes: result.stats.enumTypes.length,
						scalarTypes: result.stats.scalarTypes.length,
						relationFields: result.stats.relationFields.length,
						connectionTypes: result.stats.connectionTypes.length,
						sortInputTypes: result.stats.sortInputTypes.length,
						filterInputTypes: result.stats.filterInputTypes.length,
					},
					outputPath: normalizedOptions.output,
				},
			}
		}
	} catch (error) {
		if (error instanceof PluginError) {
			throw error
		}

		if (error instanceof Error && error.message.includes('validation')) {
			throw new PluginError('Invalid plugin options', ErrorCategory.VALIDATION, { originalError: error })
		}

		console.error('Error during GraphQL schema generation:', error)

		throw new PluginError('GraphQL schema generation failed', ErrorCategory.GENERATION, { originalError: error }, [
			'Check your ZModel schema for errors',
			'Verify plugin configuration options',
			'Ensure all required models and fields are properly defined',
		])
	}
}
