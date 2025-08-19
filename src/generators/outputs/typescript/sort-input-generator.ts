import { BaseGenerator } from '@generators/base-generator'
import { DataModel } from '@zenstackhq/sdk/ast'
import { TypeScriptGeneratorContext } from './typescript-generator-factory'

export class TypeScriptSortInputGenerator extends BaseGenerator {
	private astFactory: TypeScriptGeneratorContext['astFactory']

	constructor(context: TypeScriptGeneratorContext) {
		super(context)
		this.astFactory = context.astFactory
	}

	generate(): string[] {
		this.createSortDirectionEnum()
		this.forEachValidModel((model) => this.generateSortInputType(model))
		return []
	}

	private createSortDirectionEnum(): void {
		this.astFactory.createSortDirectionEnum()
	}

	private generateSortInputType(model: DataModel): void {
		const typeName = this.attributeProcessor.model(model).getFormattedTypeName(this.typeFormatter)
		const sortableFields = this.getSortableFields(model)

		if (sortableFields.length === 0) {
			sortableFields.push({
				name: '_placeholder',
				description: 'Placeholder field when no sortable fields are available'
			})
		}

		this.astFactory.createSortInputType(typeName, sortableFields)
	}

	private getSortableFields(model: DataModel): Array<{ name: string, description?: string }> {
		return model.fields
			.filter((field) => {
				const fieldProcessor = this.attributeProcessor.field(model, field.name)
				const isAttrSortable = fieldProcessor.isSortable()
				const isTypeSortable = fieldProcessor.isSortableType()
				const shouldInclude = fieldProcessor.shouldInclude(true)
				return isAttrSortable && isTypeSortable && shouldInclude
			})
			.map((field) => ({
				name: this.typeFormatter.formatFieldName(field.name),
				description: `Sort by ${field.name}`
			}))
	}
}