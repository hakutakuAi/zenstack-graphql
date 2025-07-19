import { GeneratorContext } from '@types'
import { BaseGenerator } from '@generators/base-generator'
import { ValidationUtils } from '@utils/schema/validation'
import { TypeKind } from '@utils/registry/unified-registry'
import { GraphQLTypeFactories } from '@utils/schema/graphql-type-factories'
import { Generate, SchemaOp, Validate } from '@utils/error'
import { DataModel, DataModelField } from '@zenstackhq/sdk/ast'

export class SortInputGenerator extends BaseGenerator {
	private models: DataModel[]
	private typeFactories: GraphQLTypeFactories

	constructor(context: GeneratorContext) {
		super(context)

		this.models = context.models
		this.typeFactories = new GraphQLTypeFactories(this.schemaComposer, this.typeFormatter)
	}

	protected override skipGeneration(): boolean {
		return !this.options.connectionTypes
	}

	@Generate({
		suggestions: ['Check model definitions in your schema', 'Ensure sort input types are properly configured'],
	})
	generate(): void {
		if (this.skipGeneration()) {
			return
		}

		this.createSortDirectionEnum()
		this.models.filter((model) => ValidationUtils.shouldGenerateModel(model, this.attributeProcessor)).forEach((model) => this.generateSortInputType(model))
	}

	@SchemaOp()
	private createSortDirectionEnum(): void {
		if (this.registry.isTypeOfKind('SortDirection', TypeKind.ENUM)) {
			return
		}

		const sortDirectionTC = this.typeFactories.createSortDirectionEnum()
		this.registry.registerType('SortDirection', TypeKind.ENUM, sortDirectionTC, true)
	}

	@SchemaOp({
		suggestions: ['Check field definitions in the model', 'Ensure fields are valid for sorting'],
	})
	private generateSortInputType(model: DataModel): void {
		const typeName = this.getObjectTypeName(model)
		const sortInputName = this.typeFormatter.formatSortInputTypeName(typeName)

		if (this.schemaComposer.has(sortInputName)) {
			return
		}

		const fields = model.fields
			.filter((field) => this.isSortableField(model, field))
			.reduce(
				(acc, field) => {
					const fieldName = this.typeFormatter.formatFieldName(field.name)
					acc[fieldName] = { description: `Sort by ${fieldName}` }
					return acc
				},
				{} as Record<string, { description: string }>
			)

		const sortInputTC = this.typeFactories.createSortInputType(typeName, fields)
		this.registry.registerType(sortInputName, TypeKind.INPUT, sortInputTC, true)
	}

	@Validate()
	private isSortableField(model: DataModel, field: DataModelField): boolean {
		const isAttrSortable = ValidationUtils.isFieldSortable(model, field.name, this.attributeProcessor)
		const isTypeSortable = ValidationUtils.isSortableFieldType(field)
		const shouldInclude = ValidationUtils.shouldIncludeField(model, field, this.attributeProcessor, true)

		return isAttrSortable && isTypeSortable && shouldInclude
	}

	private getObjectTypeName(model: DataModel): string {
		const customName = ValidationUtils.getModelName(model, this.attributeProcessor)
		return this.typeFormatter.formatTypeName(customName || model.name)
	}

	getGeneratedSortInputTypes(): string[] {
		return this.registry.getTypesByKind(TypeKind.INPUT).filter((name) => name.endsWith('SortInput'))
	}

	hasSortInputType(name: string): boolean {
		return this.registry.isTypeOfKind(name, TypeKind.INPUT) && name.endsWith('SortInput')
	}

	getSortInputTypeForModel(modelName: string): string | undefined {
		const sortInputName = this.typeFormatter.formatSortInputTypeName(modelName)
		return this.registry.isTypeOfKind(sortInputName, TypeKind.INPUT) ? sortInputName : undefined
	}
}
