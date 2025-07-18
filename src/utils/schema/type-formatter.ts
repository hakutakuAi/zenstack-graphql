import { FieldNaming, TypeNaming } from '@utils/config/options-validator'

function toPascalCase(str: string): string {
	return str.charAt(0).toUpperCase() + str.slice(1).replace(/_([a-z])/g, (_, letter) => letter.toUpperCase())
}

function toCamelCase(str: string): string {
	const pascalCase = toPascalCase(str)
	return pascalCase.charAt(0).toLowerCase() + pascalCase.slice(1)
}

function toSnakeCase(str: string): string {
	return str.replace(/[A-Z]/g, (letter) => `_${letter.toLowerCase()}`).replace(/^_/, '')
}

export class TypeFormatter {
	private readonly typeNaming: TypeNaming
	private readonly fieldNaming: FieldNaming

	constructor(typeNaming: TypeNaming, fieldNaming: FieldNaming) {
		this.typeNaming = typeNaming
		this.fieldNaming = fieldNaming
	}

	formatTypeName(name: string): string {
		switch (this.typeNaming) {
			case 'PascalCase':
				return toPascalCase(name)
			case 'camelCase':
				return toCamelCase(name)
			case 'preserve':
				return name
		}
	}

	formatFieldName(name: string): string {
		switch (this.fieldNaming) {
			case 'camelCase':
				return toCamelCase(name)
			case 'snake_case':
				return toSnakeCase(name)
			case 'preserve':
				return name
		}
	}

	formatEnumValueName(name: string): string {
		return name.toUpperCase()
	}

	formatConnectionTypeName(typeName: string): string {
		return `${this.formatTypeName(typeName)}Connection`
	}

	formatEdgeTypeName(typeName: string): string {
		return `${this.formatTypeName(typeName)}Edge`
	}

	formatSortInputTypeName(typeName: string): string {
		return `${this.formatTypeName(typeName)}SortInput`
	}

	static fromOptions(typeNaming: TypeNaming, fieldNaming: FieldNaming): TypeFormatter {
		return new TypeFormatter(typeNaming, fieldNaming)
	}
}
