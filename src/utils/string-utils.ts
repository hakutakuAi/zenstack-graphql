import { FieldNaming, TypeNaming } from '@utils/options-validator'

export function toPascalCase(str: string): string {
	return str.charAt(0).toUpperCase() + str.slice(1).replace(/_([a-z])/g, (_, letter) => letter.toUpperCase())
}

export function toCamelCase(str: string): string {
	const pascalCase = toPascalCase(str)
	return pascalCase.charAt(0).toLowerCase() + pascalCase.slice(1)
}

export function toSnakeCase(str: string): string {
	return str.replace(/[A-Z]/g, (letter) => `_${letter.toLowerCase()}`).replace(/^_/, '')
}

export function formatTypeName(name: string, convention: TypeNaming): string {
	switch (convention) {
		case 'PascalCase':
			return toPascalCase(name)
		case 'camelCase':
			return toCamelCase(name)
		case 'preserve':
			return name
	}
}

export function formatFieldName(name: string, convention: FieldNaming): string {
	switch (convention) {
		case 'camelCase':
			return toCamelCase(name)
		case 'snake_case':
			return toSnakeCase(name)
		case 'preserve':
			return name
	}
}

export function formatEnumValueName(name: string): string {
	return name.toUpperCase()
}
