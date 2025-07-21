import { Result, ok, err } from 'neverthrow'
import { BaseGenerator } from '@generators/base-generator'
import { TypeKind } from '@/utils/registry/registry'
import { DataModel } from '@zenstackhq/sdk/ast'
import { ErrorCategory, logWarning } from '@utils/error'

export class SortInputGenerator extends BaseGenerator {
	generate(): string[] {
		this.createSortDirectionEnum()

		for (const model of this.models) {
			if (!this.attributeProcessor.model(model).isIgnored()) {
				const result = this.generateSortInputType(model)
				if (result.isErr()) {
					logWarning(`Failed to generate sort input for model ${model.name}: ${result.error}`, ErrorCategory.GENERATION, {
						model: model.name,
						error: result.error,
					})
				}
			}
		}

		return this.registry.getTypesByKind(TypeKind.INPUT).filter((name) => name.endsWith('SortInput'))
	}

	private createSortDirectionEnum(): Result<void, string> {
		if (this.registry.isTypeOfKind('SortDirection', TypeKind.ENUM)) {
			return ok(undefined)
		}

		try {
			const sortDirectionTC = this.typeFactories.createSortDirectionEnum()
			this.registry.registerType('SortDirection', TypeKind.ENUM, sortDirectionTC, true)
			return ok(undefined)
		} catch (error) {
			return err(`Failed to create SortDirection enum: ${error instanceof Error ? error.message : String(error)}`)
		}
	}

	private generateSortInputType(model: DataModel): Result<void, string> {
		const typeName = this.attributeProcessor.model(model).getFormattedTypeName(this.typeFormatter)
		const sortInputName = this.typeFormatter.formatSortInputTypeName(typeName)

		if (this.schemaComposer.has(sortInputName)) {
			return ok(undefined)
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
			return ok(undefined)
		} catch (error) {
			return err(`Error creating sort input type for ${model.name}: ${error instanceof Error ? error.message : String(error)}`)
		}
	}
}
