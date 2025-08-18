import { BaseGenerator } from '@generators/base-generator'
import { TypeKind } from '@utils/registry'
import { Enum } from '@zenstackhq/sdk/ast'

export interface EnumValueConfig {
	value: string
	description?: string
}

export class EnumGenerator extends BaseGenerator {
	generate(): string[] {
		this.enums.forEach((enumObj) => this.generateEnum(enumObj))
		return this.registry.getEnumTypes()
	}

	private generateEnum(enumObj: Enum): void {
		const enumName = this.getEnumName(enumObj)

		if (this.hasEnum(enumName)) {
			return
		}

		const enumValues = this.createEnumValues(enumObj)
		const enumDescription = this.getEnumDescription(enumObj)

		const enumComposer = this.schemaComposer.createEnumTC({
			name: enumName,
			description: enumDescription,
			values: enumValues,
		})

		this.schemaComposer.set(enumName, enumComposer)
		this.registry.registerType(enumName, TypeKind.ENUM, enumComposer, true)
	}

	private getEnumName(enumObj: Enum): string {
		return this.typeFormatter.formatTypeName(enumObj.name)
	}

	private getEnumDescription(enumObj: Enum): string | undefined {
		return enumObj.comments?.[0]
	}

	private createEnumValues(enumObj: Enum): Record<string, EnumValueConfig> {
		const values: Record<string, EnumValueConfig> = {}

		for (const field of enumObj.fields) {
			const valueName = this.getEnumValueNameFromField(field)

			values[valueName] = {
				value: field.name,
			}
		}

		return values
	}

	private getEnumValueNameFromField(field: any): string {
		return this.typeFormatter.formatEnumValueName(field.name)
	}

	hasEnum(name: string): boolean {
		return this.registry.isTypeOfKind(name, TypeKind.ENUM)
	}
}
