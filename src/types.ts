import { SchemaComposer } from 'graphql-compose'
import { Model, DataModel, DataModelField } from '@zenstackhq/sdk/ast'
import { DMMF } from '@zenstackhq/sdk/prisma'
import { NormalizedOptions } from '@utils/options-validator'
import { ErrorHandler } from '@utils/error-handler'
import { AttributeProcessor } from '@utils/attribute-processor'
import { TypeMapper } from '@utils/type-mapper'

export type ComposerType = ReturnType<SchemaComposer['get']>

export interface BaseGeneratorContext {
	schemaComposer: SchemaComposer<unknown>
	options: NormalizedOptions
	errorHandler: ErrorHandler
	attributeProcessor: AttributeProcessor
}

export interface TypedGeneratorContext extends BaseGeneratorContext {
	typeMapper: TypeMapper
}

export interface ModelBasedGeneratorContext extends TypedGeneratorContext {
	dmmfModels: readonly DMMF.Model[]
}

export interface EnumGeneratorContext extends BaseGeneratorContext {
	dmmfEnums: readonly DMMF.DatamodelEnum[]
}

export interface CoreGeneratorContext extends TypedGeneratorContext {
	model: Model
	dmmf: DMMF.Document
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

export type FieldResolver<TSource = unknown, TContext = unknown, TArgs = Record<string, unknown>> = (source: TSource, args: TArgs, context: TContext) => unknown | Promise<unknown>

export type { Model, DataModel, DataModelField, DMMF }
