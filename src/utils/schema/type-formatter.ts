import { FieldNaming, TypeNaming } from '@utils/config'
import { pascalCase, camelCase, snakeCase, constantCase } from 'change-case'

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
				return pascalCase(name)
			case 'camelCase':
				return camelCase(name)
			case 'preserve':
				return name
		}
	}

	formatFieldName(name: string): string {
		switch (this.fieldNaming) {
			case 'camelCase':
				return camelCase(name)
			case 'snake_case':
				return snakeCase(name)
			case 'preserve':
				return name
		}
	}

	formatEnumValueName(name: string): string {
		return constantCase(name)
	}

	formatNameWithSuffix(typeName: string, suffix: string): string {
		return `${this.formatTypeName(typeName)}${suffix}`
	}

	appendSuffix(formattedTypeName: string, suffix: string): string {
		return `${formattedTypeName}${suffix}`
	}

	formatConnectionTypeName(typeName: string): string {
		if (typeName.endsWith('Connection')) {
			return typeName
		}
		return this.formatNameWithSuffix(typeName, 'Connection')
	}

	formatEdgeTypeName(typeName: string): string {
		if (typeName.endsWith('Edge')) {
			return typeName
		}
		return this.formatNameWithSuffix(typeName, 'Edge')
	}

	formatSortInputTypeName(typeName: string): string {
		if (typeName.endsWith('SortInput')) {
			return typeName
		}
		return this.formatNameWithSuffix(typeName, 'SortInput')
	}

	formatFilterInputTypeName(typeName: string): string {
		if (typeName.endsWith('FilterInput')) {
			return typeName
		}
		return this.formatNameWithSuffix(typeName, 'FilterInput')
	}

	formatCreateInputTypeName(typeName: string): string {
		if (typeName.endsWith('CreateInput')) {
			return typeName
		}
		return this.formatNameWithSuffix(typeName, 'CreateInput')
	}

	formatUpdateInputTypeName(typeName: string): string {
		if (typeName.endsWith('UpdateInput')) {
			return typeName
		}
		return this.formatNameWithSuffix(typeName, 'UpdateInput')
	}

	static fromOptions(typeNaming: TypeNaming, fieldNaming: FieldNaming): TypeFormatter {
		return new TypeFormatter(typeNaming, fieldNaming)
	}
}
