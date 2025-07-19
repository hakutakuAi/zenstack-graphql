import { EnumTypeComposer } from 'graphql-compose'
import { BaseGenerator } from '@generators/base-generator'
import { GeneratorContext } from '@types'
import { TypeKind } from '@/utils/registry/registry'
import { Enum } from '@zenstackhq/sdk/ast'

export interface EnumValueConfig {
	value: string
	description?: string
}

export class EnumGenerator extends BaseGenerator {
	private enums: Enum[]

	constructor(context: GeneratorContext) {
		super(context)
		if (!context.enums) {
			throw new Error('Enums are required for EnumGenerator')
		}
		this.enums = context.enums
	}

	protected override skipGeneration(): boolean {
		return !this.options.generateEnums
	}

	generate(): void {
		if (this.skipGeneration()) {
			return
		}

		this.enums.forEach((enumObj) => this.generateEnum(enumObj))
	}

	getGeneratedEnums(): string[] {
		return this.registry.getEnumTypes()
	}

	hasEnum(name: string): boolean {
		return this.registry.isTypeOfKind(name, TypeKind.ENUM)
	}

	getEnumComposer(name: string): EnumTypeComposer | undefined {
		return this.registry.getEnumComposer(name)
	}

	getEnumValues(enumName: string): string[] {
		return this.registry.getEnumValues(enumName)
	}

	isValidEnumValue(enumName: string, value: string): boolean {
		return this.registry.isValidEnumValue(enumName, value)
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
}
