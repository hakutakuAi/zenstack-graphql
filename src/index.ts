import 'reflect-metadata'

export * from '@/orchestrator/generators'

import type { PluginOptions as SdkPluginOptions } from '@zenstackhq/sdk'
import { DataModel, Enum, isDataModel, isEnum, type Model } from '@zenstackhq/sdk/ast'
import { ErrorCategory, PluginError } from '@utils/error'
import { validateOptions, PluginOptions } from '@utils/config'
import { GeneratorOrchestrator, OutputWriter } from '@orchestrator'
import { BaseGeneratorContext, PluginMetadata } from '@core/types'

export const name = 'ZenStack GraphQL'
export const description = 'Generates GraphQL schemas'

export default async function run(model: Model, options: SdkPluginOptions): Promise<{ metadata: PluginMetadata }> {
	validateModel(model)

	try {
		const normalizedOptions = validateOptions(options as PluginOptions)
		const context = createGeneratorContext(model, normalizedOptions)

		const orchestrator = new GeneratorOrchestrator(context, normalizedOptions.outputFormat)
		const result = await orchestrator.generate()

		const outputWriter = new OutputWriter()
		const outputPath = await outputWriter.write(result, normalizedOptions.output)

		return {
			metadata: {
				stats: result.stats,
				outputPath,
			},
		}
	} catch (error) {
		handleGenerationError(error)
	}
}

function validateModel(model: Model): void {
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
}

function createGeneratorContext(model: Model, normalizedOptions: ReturnType<typeof validateOptions>): BaseGeneratorContext {
	const models = model.declarations.filter((x) => isDataModel(x) && !x.isAbstract) as DataModel[]
	const enums = model.declarations.filter((x) => isEnum(x)) as Enum[]

	return {
		options: normalizedOptions,
		models,
		enums,
	}
}

function handleGenerationError(error: unknown): never {
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
