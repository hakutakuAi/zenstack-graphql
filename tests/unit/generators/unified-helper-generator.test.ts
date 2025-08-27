import { describe, it, expect } from 'bun:test'
import { UnifiedHelperGenerator, ModelHelper, HelperGenerationContext } from '@generators/unified/unified-helper-generator'
import { OutputFormat } from '@utils/constants'

describe('UnifiedHelperGenerator', () => {
	it('should extract model helpers correctly', () => {
		const helpers: ModelHelper[] = [
			{
				modelName: 'User',
				connectionBuilderName: 'UserConnectionBuilder',
				filterBuilderName: 'UserFilterBuilder',
				sortBuilderName: 'UserSortBuilder',
				fieldSelectionName: 'UserFieldSelection',
				includesConstName: 'USER_INCLUDES',
				relations: [
					{
						modelName: 'User',
						fieldName: 'posts',
						targetModelName: 'Post',
						targetFieldName: 'author',
						isList: true,
						isRequired: false,
					},
				],
			},
		]

		expect(helpers).toBeDefined()
		expect(helpers.length).toBe(1)
		expect(helpers[0]?.modelName).toBe('User')
		expect(helpers[0]?.relations.length).toBe(1)
	})

	it('should create helper generation context', () => {
		const context: HelperGenerationContext = {
			models: [],
			relations: [],
			outputFormat: OutputFormat.TYPE_GRAPHQL,
			attributeProcessor: {} as any,
		}

		expect(context).toBeDefined()
		expect(context.outputFormat).toBe(OutputFormat.TYPE_GRAPHQL)
		expect(Array.isArray(context.models)).toBe(true)
		expect(Array.isArray(context.relations)).toBe(true)
	})

	it('should have correct model helper structure', () => {
		const helper: ModelHelper = {
			modelName: 'User',
			connectionBuilderName: 'UserConnectionBuilder',
			filterBuilderName: 'UserFilterBuilder',
			sortBuilderName: 'UserSortBuilder',
			fieldSelectionName: 'UserFieldSelection',
			includesConstName: 'USER_INCLUDES',
			relations: [],
		}

		expect(helper.modelName).toBe('User')
		expect(helper.connectionBuilderName).toBe('UserConnectionBuilder')
		expect(helper.filterBuilderName).toBe('UserFilterBuilder')
		expect(helper.sortBuilderName).toBe('UserSortBuilder')
		expect(helper.fieldSelectionName).toBe('UserFieldSelection')
		expect(helper.includesConstName).toBe('USER_INCLUDES')
		expect(Array.isArray(helper.relations)).toBe(true)
	})
})
