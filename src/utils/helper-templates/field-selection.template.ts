export const FIELD_SELECTION_TEMPLATE = `

export interface ResolveTree {
	name: string
	alias: string
	args: { [str: string]: unknown }
	fieldsByTypeName: FieldsByTypeName
}

export interface FieldsByTypeName {
	[str: string]: { [str: string]: ResolveTree }
}

export function buildPrismaInclude(_resolveInfo: any, relations: string[] = []): any {
	const include: any = {}
	
	relations.forEach(relation => {
		include[relation] = true
	})
	
	return include
}

export class FieldSelection {
	{{MODEL_SPECIFIC_METHODS}}
}`

export const MODEL_FIELD_SELECTION_METHOD_TEMPLATE = `
	static build{{MODEL_NAME}}Include(info?: any): any {
		const relationFields = [{{RELATION_FIELDS}}]
		return buildPrismaInclude(info, relationFields)
	}`

export const INCLUDES_TEMPLATE = `{{MODEL_INCLUDES}}`

export const MODEL_INCLUDE_TEMPLATE = `
export const {{MODEL_NAME_UPPER}}_INCLUDES = {
	{{RELATION_INCLUDES}}
}`

export const RELATION_INCLUDE_TEMPLATE = `{{FIELD_NAME}}: true`