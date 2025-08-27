import { SchemaComposer } from 'graphql-compose'
import { DataModel, Enum } from '@zenstackhq/sdk/ast'
import { NormalizedOptions } from '@utils/config'
import { SchemaProcessor } from '@utils/schema/schema-processor'
import { GraphQLTypeFactories } from '@utils/schema/graphql-type-factories'
import { GraphQLRegistry } from '@utils/registry'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { UnifiedTypeMapper } from '@utils/type-mapping/unified-type-mapper'
import { OutputFormat } from '@utils/constants'

export type ComposerType = ReturnType<SchemaComposer['get']>

export interface BaseGeneratorContext {
	options: NormalizedOptions
	models: DataModel[]
	enums: Enum[]
}

export interface GeneratorContext extends BaseGeneratorContext {
	schemaComposer: SchemaComposer<unknown>
	registry: GraphQLRegistry
	attributeProcessor: SchemaProcessor
	typeFormatter: TypeFormatter
	typeMapper: UnifiedTypeMapper
	typeFactories: GraphQLTypeFactories
}

export interface GenerationResult<T = string> {
	items: T[]
	count: number
	type: GenerationType
}

export enum GenerationType {
	SCALAR = 'scalar',
	ENUM = 'enum',
	OBJECT = 'object',
	INPUT = 'input',
	CONNECTION = 'connection',
	FILTER = 'filter',
	SORT = 'sort',
	RELATION = 'relation',
	HELPER = 'helper',
}

export interface UnifiedGenerationResult {
	sdl?: string
	code?: string
	helperCode?: string
	results: GenerationResult[]
	stats: UnifiedGenerationStats
	outputFormat: OutputFormat
}

export interface UnifiedGenerationStats {
	objectTypes: number
	enumTypes: number
	scalarTypes: number
	inputTypes: number
	relationFields: number
	connectionTypes: number
	sortInputTypes: number
	filterInputTypes: number
	helperFiles: number
	totalTypes: number
	generationTimeMs: number
}

export interface PluginMetadata {
	stats: UnifiedGenerationStats
	outputPath: string
}
