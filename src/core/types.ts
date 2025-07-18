import { SchemaComposer } from 'graphql-compose'
import { Model, DataModel, DataModelField } from '@zenstackhq/sdk/ast'
import { DMMF } from '@zenstackhq/sdk/prisma'
import { NormalizedOptions } from '@utils/config/options-validator'
import { ErrorHandler } from '@utils/error/error-handler'
import { AttributeProcessor } from '@utils/schema/attribute-processor'
import { TypeMapper } from '@utils/schema/type-mapper'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { UnifiedRegistry } from '@utils/registry/unified-registry'

export type ComposerType = ReturnType<SchemaComposer['get']>

export interface GeneratorContext {
	schemaComposer: SchemaComposer<unknown>
	options: NormalizedOptions
	errorHandler: ErrorHandler
	attributeProcessor: AttributeProcessor
	typeFormatter: TypeFormatter
	registry?: UnifiedRegistry
	typeMapper?: TypeMapper
	dmmfModels?: readonly DMMF.Model[]
	dmmfEnums?: readonly DMMF.DatamodelEnum[]
	model?: Model
	dmmf?: DMMF.Document
}

export function isDataModel(model: unknown): model is DataModel {
	return typeof model === 'object' && model !== null && 'name' in model && typeof (model as DataModel).name === 'string'
}

export function asDataModel(model: unknown): DataModel {
	if (!isDataModel(model)) {
		throw new Error('Invalid model type')
	}
	return model
}

export type { Model, DataModel, DataModelField, DMMF }
