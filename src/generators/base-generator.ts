import { SchemaComposer } from 'graphql-compose'
import { ErrorHandler } from '@utils/error/error-handler'
import { NormalizedOptions } from '@utils/config/options-validator'
import { AttributeProcessor } from '@utils/schema/attribute-processor'
import { TypeMapper } from '@utils/schema/type-mapper'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { UnifiedRegistry } from '@utils/registry/unified-registry'
import { SafeOperation } from '@utils/error/error-decorators'
import { ComposerType, GeneratorContext } from '@types'

export abstract class BaseGenerator {
	protected readonly registry: UnifiedRegistry
	protected readonly schemaComposer: SchemaComposer<unknown>
	protected readonly options: NormalizedOptions
	protected readonly errorHandler: ErrorHandler
	protected readonly attributeProcessor: AttributeProcessor
	protected readonly typeFormatter: TypeFormatter
	protected readonly typeMapper?: TypeMapper

	constructor(context: GeneratorContext) {
		this.schemaComposer = context.schemaComposer
		this.options = context.options
		this.errorHandler = context.errorHandler
		this.attributeProcessor = context.attributeProcessor
		this.typeFormatter = context.typeFormatter
		this.typeMapper = context.typeMapper
		this.registry = context.registry || new UnifiedRegistry(this.schemaComposer, this.errorHandler)
	}

	abstract generate(): void

	protected skipGeneration(): boolean {
		return false
	}
}
