import { BaseGenerator } from '@generators/base-generator'
import { TypeKind } from '@/utils/registry/registry'
import { DataModel } from '@zenstackhq/sdk/ast'
import { ErrorCategory, PluginError, warning } from '@utils/error'

export class SortInputGenerator extends BaseGenerator {
	generate(): string[] {
		this.createSortDirectionEnum()

		for (const model of this.models) {
			if (!this.attributeProcessor.model(model).isIgnored()) {
				try {
					this.generateSortInputType(model)
				} catch (error) {
					if (error instanceof PluginError) {
						warning(`Failed to generate sort input for model ${model.name}: ${error.message}`, error.category, {
							model: model.name,
							error,
						})
					} else {
						warning(`Failed to generate sort input for model ${model.name}: ${error instanceof Error ? error.message : String(error)}`, ErrorCategory.GENERATION, {
							model: model.name,
							error,
						})
					}
				}
			}
		}

		return this.registry.getTypesByKind(TypeKind.INPUT).filter((name) => name.endsWith('SortInput'))
	}

	private createSortDirectionEnum(): void {
		if (this.registry.isTypeOfKind('SortDirection', TypeKind.ENUM)) {
			return
		}

		try {
			const sortDirectionTC = this.typeFactories.createSortDirectionEnum()
			this.registry.registerType('SortDirection', TypeKind.ENUM, sortDirectionTC, true)
		} catch (error) {
			throw new PluginError(`Failed to create SortDirection enum`, ErrorCategory.GENERATION, { originalError: error }, ['Check if SortDirection enum is already defined elsewhere'])
		}
	}

	private generateSortInputType(model: DataModel): void {
		const typeName = this.attributeProcessor.model(model).getFormattedTypeName(this.typeFormatter)
		const sortInputName = this.typeFormatter.formatSortInputTypeName(typeName)

		if (this.schemaComposer.has(sortInputName)) {
			return
		}

		const fields = model.fields
			.filter((field) => {
				const fieldProcessor = this.attributeProcessor.field(model, field.name)
				const isAttrSortable = fieldProcessor.isSortable()
				const isTypeSortable = fieldProcessor.isSortableType()
				const shouldInclude = fieldProcessor.shouldInclude(true)
				return isAttrSortable && isTypeSortable && shouldInclude
			})
			.reduce(
				(acc, field) => {
					const fieldName = this.attributeProcessor.field(model, field.name).getFormattedFieldName(this.typeFormatter)
					acc[fieldName] = { description: `Sort by ${fieldName}` }
					return acc
				},
				{} as Record<string, { description: string }>
			)

		try {
			const sortInputTC = this.typeFactories.createSortInputType(typeName, fields)
			this.registry.registerType(sortInputName, TypeKind.INPUT, sortInputTC, true)
		} catch (error) {
			throw new PluginError(`Error creating sort input type for ${model.name}`, ErrorCategory.GENERATION, { modelName: model.name, error }, ['Check if the model fields are defined correctly'])
		}
	}
}
