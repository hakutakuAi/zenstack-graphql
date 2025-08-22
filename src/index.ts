import 'reflect-metadata'

export * from '@generators'

import type { PluginOptions as SdkPluginOptions } from '@zenstackhq/sdk'
import { DataModel, Enum, isDataModel, isEnum, type Model } from '@zenstackhq/sdk/ast'

import { CoreGenerator } from '@generators/core-generator'
import { ErrorCategory, PluginError } from '@utils/error'
import { validateOptions, PluginOptions } from '@utils/config'
import { FileWriter } from '@utils/file-writer'
import { OutputFormat } from '@utils/constants'
import path from 'path'

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
			const { TypeFormatter } = await import('@utils/schema/type-formatter')
			const { TypeScriptASTFactory } = await import('@utils/typescript/ast-factory')
			const { SchemaProcessor } = await import('@utils/schema/schema-processor')
			const { UnifiedTypeMapper } = await import('@utils/type-mapping/unified-type-mapper')
			const { TypeScriptOutputStrategy } = await import('@generators/strategies/typescript-output-strategy')

			const typeFormatter = new TypeFormatter(normalizedOptions.typeNaming, normalizedOptions.fieldNaming)
			const astFactory = new TypeScriptASTFactory(typeFormatter)
			const outputStrategy = new TypeScriptOutputStrategy(astFactory)
			const attributeProcessor = new SchemaProcessor()
			const typeMapper = new UnifiedTypeMapper(typeFormatter, models, enums, normalizedOptions)

			const unifiedContext = {
				options: normalizedOptions,
				models,
				enums,
				typeFormatter,
				attributeProcessor,
				typeMapper,
				outputStrategy,
			}

			const objectGenerator = new (await import('@generators/unified/unified-object-type-generator')).UnifiedObjectTypeGenerator(unifiedContext)
			const filterGenerator = new (await import('@generators/unified/unified-filter-input-generator')).UnifiedFilterInputGenerator(unifiedContext)
			const sortGenerator = new (await import('@generators/unified/unified-sort-input-generator')).UnifiedSortInputGenerator(unifiedContext)
			const connectionGenerator = new (await import('@generators/unified/unified-connection-generator')).UnifiedConnectionGenerator(unifiedContext)
			const { OutputFormat: OF } = await import('@utils/constants')
			const enumGenerator = new (await import('@generators/unified/unified-enum-generator')).UnifiedEnumGenerator(
				{ options: normalizedOptions, models, enums },
				OF.TYPE_GRAPHQL,
				typeFormatter,
			)
			const scalarGenerator = new (await import('@generators/unified/unified-scalar-generator')).UnifiedScalarGenerator(
				{ options: normalizedOptions, models, enums },
				OF.TYPE_GRAPHQL,
			)

			const scalarResult = normalizedOptions.generateScalars ? scalarGenerator.generate() : { typescriptTypes: [] }
			const enumResult = normalizedOptions.generateEnums ? enumGenerator.generate() : { typescriptTypes: [] }
			const objectResult = objectGenerator.generate()
			const filterResult = normalizedOptions.generateFilters ? filterGenerator.generate() : []
			const sortResult = normalizedOptions.generateSorts ? sortGenerator.generate() : []
			const connectionResult = normalizedOptions.connectionTypes ? connectionGenerator.generate() : []

			const outputPath = path.join(
				path.dirname(normalizedOptions.output),
				path.basename(normalizedOptions.output, path.extname(normalizedOptions.output)) + '.ts',
			)

			const code = outputStrategy.getGeneratedCode()

			await FileWriter.create().writeTypeGraphQL(code, outputPath)

			return {
				metadata: {
					stats: {
						objectTypes: objectResult.length,
						enumTypes: enumResult.typescriptTypes.length,
						scalarTypes: scalarResult.typescriptTypes.length,
						relationFields: 0,
						connectionTypes: connectionResult.length,
						sortInputTypes: sortResult.length,
						filterInputTypes: filterResult.length,
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
